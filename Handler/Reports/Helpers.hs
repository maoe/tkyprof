module Handler.Reports.Helpers
  ( runReports
  , getReports'
  , getAllReports
  , getAllProfilingReports
  , getProfilingReport
  , postProfilingReport
  ) where

import Control.Applicative
import Control.Monad.STM (STM, atomically)
import Control.Monad.Trans (liftIO)
import Model (Reports(..), ReportID, allReports, lookupReport, insertReport)
import TKYProf (Handler, TKYProf(getReports))
import Yesod.Core (getYesod)
import Yesod.Core.Handler (notFound)

import GHC.RTS.TimeAllocProfile (TimeAllocProfile)

runReports :: STM a -> Handler a
runReports = liftIO . atomically

getReports' :: Handler Reports
getReports' = getReports <$> getYesod

getAllReports :: Handler [(ReportID, TimeAllocProfile)]
getAllReports = do
  rs <- getReports'
  runReports $ allReports rs

getAllProfilingReports :: Handler [TimeAllocProfile]
getAllProfilingReports = map snd <$> getAllReports

getProfilingReport :: ReportID -> Handler TimeAllocProfile
getProfilingReport reportId = do
  rs <- getReports'
  mreport <- runReports $ lookupReport reportId rs
  case mreport of
    Just r  -> return r
    Nothing -> notFound

postProfilingReport :: TimeAllocProfile -> Handler ReportID
postProfilingReport prof = do
  rs <- getReports'
  runReports $ insertReport prof rs
