{-# LANGUAGE RecordWildCards #-}
module Model where
import Control.Applicative
import Control.Concurrent.STM (STM, TVar, newTVar, readTVar, writeTVar)
import Data.Map (Map)
import qualified Data.Map as M

import GHC.RTS.TimeAllocProfile

type ReportID = Integer

data Reports = Reports
  { newReportId :: TVar ReportID
  , reports     :: TVar (Map ReportID TimeAllocProfile)
  }

emptyReports :: STM Reports
emptyReports = do
  uid <- newTVar 0
  rs  <- newTVar M.empty
  return $ Reports { newReportId = uid
                   , reports     = rs }

insertReport :: TimeAllocProfile -> Reports -> STM ReportID
insertReport r Reports{..} = do
  uid <- readTVar newReportId
  rs  <- readTVar reports
  writeTVar newReportId (succ uid)
  writeTVar reports (M.insert uid r rs)
  return uid

deleteReport :: ReportID -> Reports -> STM ()
deleteReport i Reports{..} = do
  rs <- readTVar reports
  writeTVar reports (M.delete i rs)

lookupReport :: ReportID -> Reports -> STM (Maybe TimeAllocProfile)
lookupReport i Reports{..} = do
  rs <- readTVar reports
  return $ M.lookup i rs

memberReport :: ReportID -> Reports -> STM Bool
memberReport i Reports{..} = do
  rs <- readTVar reports
  return $ M.member i rs

allReports :: Reports -> STM [(ReportID, TimeAllocProfile)]
allReports (Reports _ rs) = M.toList <$> readTVar rs

allProfilingReports :: Reports -> STM [TimeAllocProfile]
allProfilingReports r = map snd <$> allReports r
