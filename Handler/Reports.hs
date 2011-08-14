{-# LANGUAGE RecordWildCards, NamedFieldPuns, TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Reports
  ( getReportsR
  , postReportsR
  , getReportsIdTimeR
  , getReportsIdAllocR
  ) where

import Control.Applicative
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
  FileInfo {fileContent} <- getPostedReport
  prof <- parseFileContent fileContent
  postProfilingReport prof

getReportsIdR :: ReportID -> Handler RepHtml
getReportsIdR reportId = do
  report@ProfilingReport {..} <- getProfilingReport reportId
  let json = T.decodeUtf8 $ A.encode reportCostCentres
  defaultLayout $ do
    setTitle $ "TKYProf Reports"
    addScript $ StaticR js_d3_min_js
    addScript $ StaticR js_d3_layout_min_js
    addWidget $(widgetFile "reports-id")

getReportsIdAllocR :: ReportID -> [a] -> Handler RepHtml
getReportsIdAllocR = const . getReportsIdR

getReportsIdTimeR :: ReportID -> [a] -> Handler RepHtml
getReportsIdTimeR = const . getReportsIdR

-- Helper functions
getPostedReport :: Handler FileInfo
getPostedReport = do
  (_, files) <- runRequestBody
  case lookup "reports" files of
    Nothing   -> invalidArgs ["Missing files"]
    Just file -> return file

parseFileContent :: L.ByteString -> Handler ProfilingReport
parseFileContent content =
  case A.parseOnly profilingReport (S.concat $ L.toChunks content) of
    Left err   -> invalidArgs ["Invalid format", toMessage err]
    Right tree -> return tree
