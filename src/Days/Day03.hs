module Days.Day03 (runDay) where

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
inputParser = many' (choice [mulParser, doParser, dontParser, fallback])

mulParser :: Parser (Maybe Inst)
mulParser = do
    _ <- string "mul("
    l <- decimal
    _ <- char ','
    r <- decimal
    _ <- char ')'
    return (Just (Mult (l,r)))

doParser :: Parser (Maybe Inst)
doParser = do
    _ <- string "do()"
    return (Just Do)

dontParser = do 
    _ <- string "don't()"
    return (Just Dont)

fallback :: Parser (Maybe Inst)
fallback = do
    _ <- anyChar
    return Nothing

------------ TYPES ------------
type Input = [Maybe Inst]
-- type Mult = (Int, Int)

data Inst = Mult (Int, Int)
    | Do
    | Dont
    deriving (Eq, Show)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA input = sum (map doInst validMul)
    where 
        validMul = catMaybes input

doInst :: Inst -> Int
doInst (Mult (a,b)) = a*b
doInst Do = 0
doInst Dont = 0 

------------ PART B ------------
partB :: Input -> OutputB
partB input = processInst (catMaybes input)

isNotDo :: Inst -> Bool
isNotDo Do = False
isNotDo _ = True

processInst :: [Inst] -> Int
processInst ((Mult (a,b)):xs) = (a*b) + (processInst xs) 
processInst (Do:xs) = processInst xs
processInst (Dont:xs) = processInst (dropWhile isNotDo xs)
processInst _ = 0