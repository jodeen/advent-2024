module Days.Day02 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}
{- HLINT ignore "Redundant bracket" -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parseLine `sepBy` (char '\n')

parseLine = decimal `sepBy` (char ' ')

------------ TYPES ------------
type Input = [Report]

type Report = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------

isSafePair :: (Int, Int) -> Bool
isSafePair (a,b) = diff >= 1 && diff <= 3
    where
        diff = abs (a - b)


isSafeReport :: Report -> Bool
isSafeReport report = pairWiseSafe && (isAscending || isDecending)
    where
        pairWiseSafe = all isSafePair pairs
        isAscending  = all (uncurry (>)) pairs
        isDecending = all (uncurry (<)) pairs
        pairs = zip report (tail report)

partA :: Input -> OutputA
partA input = length (filter isSafeReport input)

------------ PART B ------------

subLists :: Report -> [Report]
subLists report = map (removeAt report) [0..((length report)-1)]

removeAt :: Report -> Int -> Report
removeAt xs idx = h ++ t
    where (h, (_:t)) = splitAt idx xs

isSafeReportDampened :: Report -> Bool
isSafeReportDampened report = any isSafeReport (subLists report)

partB :: Input -> OutputB
partB input = length (filter isSafeReportDampened input)
