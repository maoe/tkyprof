{-# LANGUAGE FlexibleInstances, RecordWildCards, OverloadedStrings, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ProfilingReport
  ( -- * Parsers for profiling reports
    profilingReport
  , profilingReportI
    -- * Re-exported modules
  , module Data.Tree
  ) where

import Data.Aeson
import Data.Attoparsec.Text (Parser)
import Data.Conduit.Attoparsec (sinkParser)
import Data.Conduit
import Data.Tree (Tree(..), Forest)
import GHC.RTS.TimeAllocProfile
import Prelude hiding (takeWhile)
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
import Data.Text (Text)

profilingReportI :: MonadThrow m => Sink Text m TimeAllocProfile
profilingReportI = sinkParser profilingReport

profilingReport :: Parser TimeAllocProfile
profilingReport = timeAllocProfile

-- Orphan instances for Aeson
instance ToJSON TimeAllocProfile where
  toJSON prof@TimeAllocProfile {..} =
    object [ "timestamp"      .= show profileTimestamp
           , "commandLine"    .= profileCommandLine
           , "totalTime"      .= profileTotalTime
           , "totalAlloc"     .= profileTotalAlloc
           , "hotCostCentres" .= profileHotCostCentres
           , "costCentres"    .= profileCostCentres prof
           ]

instance ToJSON TotalTime where
  toJSON TotalTime {..} =
    object [ "secs"       .= show totalTimeElapsed
           , "ticks"      .= totalTimeTicks
           , "resolution" .= show totalTimeResolution
           , "processors" .= totalTimeProcessors
           ]

instance ToJSON TotalAlloc where
  toJSON TotalAlloc {..} =
    object [ "bytes" .= totalAllocBytes ]

instance ToJSON BriefCostCentre where
  toJSON BriefCostCentre {..} =
    object [ "name"   .= briefCostCentreName
           , "module" .= briefCostCentreModule
           , "time"   .= briefCostCentreTime
           , "alloc"  .= briefCostCentreAlloc
           , "ticks"  .= briefCostCentreTicks
           , "bytes"  .= briefCostCentreBytes
           ]

instance ToJSON (Tree CostCentre) where
  toJSON (Node cc@(CostCentre {..}) subForest)
    | null subForest = cc'
    | otherwise      = branch
    where
      branch = Object $ M.insert "subForest" subForestWithParent unwrappedCC
      parent = Object $ M.insert "isParent" (toJSON True) unwrappedCC
      subForestWithParent = Array $ V.fromList $ parent:map toJSON subForest
      cc'@(Object unwrappedCC) = toJSON cc

instance ToJSON CostCentre where
  toJSON CostCentre {..} =
    object [ "name"            .= costCentreName
           , "module"          .= costCentreModule
           , "no"              .= costCentreNo
           , "entries"         .= costCentreEntries
           , "individualTime"  .= costCentreIndTime
           , "individualAlloc" .= costCentreIndAlloc
           , "inheritedTime"   .= costCentreInhTime
           , "inheritedAlloc"  .= costCentreInhAlloc
           , "ticks" .= costCentreTicks
           , "bytes" .= costCentreBytes
           ]
