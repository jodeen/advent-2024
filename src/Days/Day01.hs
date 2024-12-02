module Days.Day01 (runDay) where

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
parseLine = do 
    left <- decimal
    skipSpace
    right <- decimal 
    return (left, right)  



------------ TYPES ------------
type Input = [(Int, Int)]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA input = sum (zipWith (\a b -> abs (a - b)) (sort l) (sort r))
    where 
        (l, r) = (unzip input)

------------ PART B ------------
partB :: Input -> OutputB
partB input = sum (map (\i -> i * (Map.findWithDefault 0 i freqRight)) l)
    where 
        (l, r) = (unzip input)
        freqRight = U.freq r
