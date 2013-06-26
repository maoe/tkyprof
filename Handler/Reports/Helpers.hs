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
import ProfilingReport (ProfilingReport)
import TKYProf (Handler, TKYProf(getReports))
import Yesod.Core (getYesod)
import Yesod.Core.Handler (notFound)

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

postProfilingReport :: ProfilingReport -> Handler ReportID
postProfilingReport prof = do
  rs <- getReports'
  runReports $ insertReport prof rs
