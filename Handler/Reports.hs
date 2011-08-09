{-# LANGUAGE NamedFieldPuns, TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Reports
  ( getReportsR
  , postReportsR
  , getReportsIdR
  ) where

import TKYProf
import ProfilingReport
import Control.Applicative
import Yesod.Request
import qualified Data.Attoparsec as A
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Aeson as A (encode)
import qualified Data.Text.Lazy.Encoding as T (decodeUtf8)

getReportsR :: Handler RepHtml
getReportsR = do
  reports <- getAllReports
  defaultLayout $ do
    setTitle "Devel.TKYProf Report"
    addWidget $(widgetFile "reports")

postReportsR :: Handler ()
postReportsR = do
  FileInfo {fileContent} <- getPostedReport
  prof <- parseFileContent fileContent
  postProfilingReport prof

getReportsIdR :: ReportID -> Handler RepHtml
getReportsIdR reportId = do
  report <- getProfilingReport reportId
  let json = T.decodeUtf8 $ A.encode $ reportCostCentres report
  defaultLayout $ do
    setTitle "Devel.TKYProf Report"
    addScript $ StaticR js_d3_js
    addScript $ StaticR js_d3_layout_js
    addWidget $(widgetFile "reports-id")

-- Helper functions
runReports :: STM a -> Handler a
runReports = liftIO . atomically

getReports' :: Handler Reports
getReports' = getReports <$> getYesod

getAllReports :: Handler [(ReportID, ProfilingReport)]
getAllReports = do
  rs <- getReports'
  runReports $ allReports rs

getAllProfilingReports :: Handler [ProfilingReport]
getAllProfilingReports = map snd <$> getAllReports

getProfilingReport :: ReportID -> Handler ProfilingReport
getProfilingReport reportId = do
  rs <- getReports'
  mreport <- runReports $ lookupReport reportId rs
  case mreport of
    Just r  -> return r
    Nothing -> notFound

postProfilingReport :: ProfilingReport -> Handler ()
postProfilingReport prof = do
  rs <- getReports'
  reportId <- runReports $ insertReport prof rs
  sendResponseCreated (ReportsIdR reportId)

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
