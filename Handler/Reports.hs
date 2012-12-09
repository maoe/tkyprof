{-# LANGUAGE RecordWildCards, NamedFieldPuns, TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Reports
  ( getReportsR
  , postReportsR
  , getReportsIdR
  , getReportsIdTimeR
  , getReportsIdAllocR
  ) where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Handler.Reports.Helpers (getAllReports, getProfilingReport, postProfilingReport)
import ProfilingReport
import TKYProf hiding (lift)
import Yesod.Core (lift)
import Yesod.Request
import qualified Data.Aeson as A (encode)
import qualified Data.Attoparsec as A
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy.Encoding as T (decodeUtf8)
import Network.HTTP.Types.Status (seeOther303)
import Control.Applicative ((<$>))
import Data.Conduit (($$))
import Data.Conduit.List (consume)
import Text.Julius

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
getReportsIdR reportId = redirectWith seeOther303 (ReportsIdTimeR reportId [])

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
postFileInfo info = do
  lbs <- lift $ L.fromChunks <$> (fileSource info $$ consume)
  prof <- parseFileContent lbs
  postProfilingReport prof

parseFileContent :: L.ByteString -> Handler ProfilingReport
parseFileContent content =
  case A.parseOnly profilingReport (S.concat $ L.toChunks content) of
    Left err   -> invalidArgs ["Invalid format", toMessage err]
    Right tree -> return tree

getReportsIdCommon :: ReportID -> Text -> Handler RepHtml
getReportsIdCommon reportId profilingType = do
  report@ProfilingReport {..} <- getProfilingReport reportId
  let json = rawJS $ T.decodeUtf8 $ A.encode reportCostCentres
  defaultLayout $ do
    setTitle $ "TKYProf Reports"
    addScript $ StaticR js_tkyprof_js
    addScript $ StaticR js_d3_min_js
    addScript $ StaticR js_d3_layout_min_js
    addWidget $ toWidget $(juliusFile "templates/reports-id.julius")
    addWidget $(widgetFile "reports-id")
