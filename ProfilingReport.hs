{-# LANGUAGE FlexibleInstances, RecordWildCards, OverloadedStrings, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module ProfilingReport
  ( -- * Parsers for profiling reports
    profilingReport
  , profilingReportI
    -- * Parsers for sub-parts of the report
  , timestamp
  , title
  , commandLine 
  , totalTime
  , totalAlloc
  , hotCostCentres
  , costCentres
    -- * Data types
  , ProfilingReport(..)
  , Timestamp
  , CommandLine
  , TotalTime(..)
  , TotalAlloc(..)
  , BriefCostCentre(..)
  , CostCentre(..)
    -- * Re-exported modules
  , module Data.Tree
  ) where

import Control.Applicative hiding (many)
import Data.Aeson
import Data.Attoparsec.Char8 as A8
import Data.Attoparsec.Enumerator (iterParser)
import Data.ByteString (ByteString)
import Data.Enumerator (Iteratee)
import Data.Foldable (foldl')
import Data.Time (UTCTime(..), TimeOfDay(..), timeOfDayToTime, fromGregorian)
import Data.Tree (Tree(..), Forest)
import Data.Tree.Zipper (TreePos, Full)
import Prelude hiding (takeWhile)
import qualified Data.Attoparsec as A
import qualified Data.Map as M
import qualified Data.Tree.Zipper as Z
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text.Encoding as T

data ProfilingReport = ProfilingReport
  { reportTimestamp      :: Timestamp
  , reportCommandLine    :: CommandLine
  , reportTotalTime      :: TotalTime
  , reportTotalAlloc     :: TotalAlloc
  , reportHotCostCentres :: [BriefCostCentre]
  , reportCostCentres    :: Tree CostCentre
  } deriving Show

type Timestamp = UTCTime

type CommandLine = Text

data TotalTime = TotalTime
  { totalSecs  :: Double
  , totalTicks :: Integer
  , resolution :: Integer
  } deriving Show

newtype TotalAlloc = TotalAlloc
  { totalAllocBytes :: Integer
  } deriving Show

data BriefCostCentre = BriefCostCentre
  { briefCostCentreName   :: Text
  , briefCostCentreModule :: Text
  , briefCostCentreTime   :: Double
  , briefCostCentreAlloc  :: Double
  } deriving Show

data CostCentre = CostCentre
  { costCentreName    :: ByteString
  , costCentreModule  :: ByteString
  , costCentreNo      :: Integer
  , costCentreEntries :: Integer
  , individualTime    :: Double
  , individualAlloc   :: Double
  , inheritedTime     :: Double
  , inheritedAlloc    :: Double
  } deriving Show

profilingReportI :: Monad m => Iteratee ByteString m ProfilingReport
profilingReportI = iterParser profilingReport

profilingReport :: Parser ProfilingReport
profilingReport = spaces >>
  ProfilingReport <$> timestamp
                  <*  title          <* spaces
                  <*> commandLine    <* spaces
                  <*> totalTime      <* spaces
                  <*> totalAlloc     <* spaces
                  <*> hotCostCentres <* spaces
                  <*> costCentres

timestamp :: Parser Timestamp
timestamp = do
  dayOfTheWeek     <* spaces
  m   <- month     <* spaces
  d   <- day       <* spaces
  tod <- timeOfDay <* spaces
  y   <- year      <* spaces
  return UTCTime { utctDay     = fromGregorian y m d
                 , utctDayTime = timeOfDayToTime tod }
  where
    year = decimal
    month = toNum <$> A8.take 3
      where toNum m = case m of
                        "Jan" -> 1; "Feb" -> 2; "Mar" -> 3; "Apr" -> 4;
                        "May" -> 5; "Jun" -> 6; "Jul" -> 7; "Aug" -> 8;
                        "Sep" -> 9; "Oct" -> 10; "Nov" -> 11; "Dec" -> 12
                        _ -> error "timestamp.toNum: impossible"
    day = decimal
    timeOfDay = TimeOfDay <$> decimal <* string ":" <*> decimal <*> pure 0
    dayOfTheWeek = takeTill isSpace

title :: Parser ByteString
title = string "Time and Allocation Profiling Report  (Final)"

commandLine :: Parser CommandLine
commandLine = T.decodeUtf8 <$> line

totalTime :: Parser TotalTime
totalTime = do
  string "total time  ="; spaces
  secs <- double
  string " secs"; spaces
  (ticks, res) <- parens $
    (,) <$> decimal <* string " ticks @ "
        <*> decimal <* string " ms"
  return TotalTime { totalSecs  = secs
                   , totalTicks = ticks
                   , resolution = res }

totalAlloc :: Parser TotalAlloc
totalAlloc = do
  string "total alloc ="; spaces
  n <- groupedDecimal
  string " bytes" <* spaces <* parens (string "excludes profiling overheads")
  return TotalAlloc { totalAllocBytes = n }

groupedDecimal :: Parser Integer
groupedDecimal = foldl' go 0 <$> decimal `sepBy` char8 ','
  where go z n = z*1000 + n

hotCostCentres :: Parser [BriefCostCentre]
hotCostCentres = header *> spaces *> many1 briefCostCentre
  where header :: Parser ByteString
        header = line

briefCostCentre :: Parser BriefCostCentre
briefCostCentre =
  BriefCostCentre <$> symbolText <* spaces
                  <*> symbolText <* spaces
                  <*> double     <* spaces
                  <*> double     <* spaces

costCentres :: Parser (Tree CostCentre)
costCentres = header *> spaces *> costCentreTree
  where header = count 2 line

-- Internal functions
costCentreTree :: Parser (Tree CostCentre)
costCentreTree = buildTree <$> costCentreMap >>= maybe empty pure
  where
    costCentreMap = nestedCostCentre `sepBy1` endOfLine
    nestedCostCentre = (,) <$> nestLevel <*> costCentre

nestLevel :: Parser Int
nestLevel = howMany space

costCentre :: Parser CostCentre
costCentre = CostCentre <$> takeWhile (not . isSpace) <* spaces
                        <*> takeWhile (not . isSpace) <* spaces
                        <*> decimal                   <* spaces
                        <*> decimal                   <* spaces
                        <*> double                    <* spaces
                        <*> double                    <* spaces
                        <*> double                    <* spaces
                        <*> double

type Zipper = TreePos Full
type Level = Int

buildTree :: [(Level, a)] -> Maybe (Tree a)
buildTree [] = Nothing
buildTree ((lvl, t):xs) = Z.toTree <$> snd (foldl' go (lvl, Just z) xs)
  where
    z = Z.fromTree $ Node t []
    go :: (Level, Maybe (Zipper a)) -> (Level, a) -> (Level, Maybe (Zipper a))
    go (curLvl, mzipper) a@(lvl', x)
      | curLvl > lvl' = go (curLvl-1, mzipper >>= Z.parent) a
      | curLvl < lvl' = case mzipper >>= Z.lastChild of
                          Nothing  -> (lvl', Z.insert (Node x []) . Z.children <$> mzipper)
                          mzipper' -> go (curLvl+1, mzipper') a
      | otherwise     = (lvl', Z.insert (Node x []) . Z.nextSpace <$> mzipper)


-- Small utilities
howMany :: Parser a -> Parser Int
howMany p = howMany' 0
  where howMany' !n = (p >> howMany' (succ n)) <|> return n

spaces :: Parser ()
spaces = () <$ many space

line :: Parser ByteString
line = A.takeWhile (not . isEndOfLine) <* spaces

parens :: Parser a -> Parser a
parens p = string "(" *> p <* string ")"

symbol :: Parser ByteString
symbol = takeWhile (not . isSpace)

symbolText :: Parser Text
symbolText = T.decodeUtf8 <$> symbol

-- Aeson
instance ToJSON ProfilingReport where
  toJSON ProfilingReport {..} =
    object [ "timestamp"      .= reportTimestamp
           , "commandLine"    .= reportCommandLine
           , "totalTime"      .= reportTotalTime
           , "totalAlloc"     .= reportTotalAlloc
           , "hotCostCentres" .= reportHotCostCentres
           , "costCentres"    .= reportCostCentres
           ]

instance ToJSON TotalTime where
  toJSON TotalTime {..} =
    object [ "secs"       .= totalSecs
           , "ticks"      .= totalTicks
           , "resolution" .= resolution
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
           , "individualTime"  .= individualTime
           , "individualAlloc" .= individualAlloc
           , "inheritedTime"   .= inheritedTime
           , "inheritedAlloc"  .= inheritedAlloc ]
