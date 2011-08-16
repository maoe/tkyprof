{-# LANGUAGE RecordWildCards, NamedFieldPuns, TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Reports
  ( getReportsR
  , postReportsR
  , getReportsIdR
  , getReportsIdTimeR
  , getReportsIdAllocR
  ) where

import Control.Applicative
import Control.Monad (forM_)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Handler.Reports.Helpers (getAllReports, getProfilingReport, postProfilingReport)
import ProfilingReport
import TKYProf
import Yesod.Request
import qualified Data.Aeson as A (encode)
import qualified Data.Attoparsec as A
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy.Encoding as T (decodeUtf8)

getReportsR :: Handler RepHtml
getReportsR = do
  reports <- getAllReports
  defaultLayout $ do
    setTitle "TKYProf Reports"
    addWidget $(widgetFile "reports")

postReportsR :: Handler ()
postReportsR = do
  files <- getPostedReports
  case files of
    [fileInfo] -> do reportId <- postFileInfo fileInfo
                     sendResponseCreated $ ReportsIdR reportId
    _          -> do mapM_ postFileInfo files
                     sendResponseCreated ReportsR

getReportsIdR :: ReportID -> Handler RepHtml
getReportsIdR reportId = redirect RedirectSeeOther (ReportsIdTimeR reportId [])

getReportsIdTimeR :: ReportID -> [a] -> Handler RepHtml
getReportsIdTimeR reportId _ = getReportsIdCommon reportId "time"

getReportsIdAllocR :: ReportID -> [a] -> Handler RepHtml
getReportsIdAllocR reportId _ = getReportsIdCommon reportId "alloc"

-- Helper functions
getPostedReports :: Handler [FileInfo]
getPostedReports = do
  (_, files) <- runRequestBody
  case [ file | (header, file) <- files, header == "reports" ] of
    []    -> invalidArgs ["Missing files"]
    found -> return found

postFileInfo :: FileInfo -> Handler ReportID
postFileInfo FileInfo {fileContent} = do
  prof <- parseFileContent fileContent
  postProfilingReport prof

parseFileContent :: L.ByteString -> Handler ProfilingReport
parseFileContent content =
  case A.parseOnly profilingReport (S.concat $ L.toChunks content) of
    Left err   -> invalidArgs ["Invalid format", toMessage err]
    Right tree -> return tree

getReportsIdCommon :: ReportID -> Text -> Handler RepHtml
getReportsIdCommon reportId profilingType = do
  report@ProfilingReport {..} <- getProfilingReport reportId
  let json = T.decodeUtf8 $ A.encode reportCostCentres
  defaultLayout $ do
    setTitle $ "TKYProf Reports"
    addScript $ StaticR js_tkyprof_js
    addScript $ StaticR js_d3_min_js
    addScript $ StaticR js_d3_layout_min_js
    addWidget $(widgetFile "reports-id")
