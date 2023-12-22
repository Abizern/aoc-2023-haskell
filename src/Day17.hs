-- usage: cabal run aoc23 17
-- description: I used A* rather than Djikstra, because it's easier with the Algorithm.Search package.

module Day17 where

import AOCUtils.Input (makeTitle)
import Algorithm.Search (aStar, pruning)
import Data.Char (digitToInt)
import Data.Matrix (Matrix, fromLists, ncols, nrows, (!))
import Data.Maybe (fromJust)

type Grid = Matrix Int

data Direction = North | East | South | West deriving (Eq, Show, Ord)

data State = State
  { position :: (Int, Int),
    direction :: Direction,
    stepCount :: Int
  }
  deriving (Eq, Show, Ord)

allDirections :: [Direction]
allDirections = [North, West, South, East]

opposite :: Direction -> Direction
opposite North = South
opposite East = West
opposite South = North
opposite West = East

move :: State -> Direction -> State
move (State (row, col) dir cnt) newDir = State newPos newDir newCnt
  where
    newPos = case newDir of
      North -> (pred row, col)
      East -> (row, succ col)
      South -> (succ row, col)
      West -> (row, pred col)
    newCnt
      | cnt == 0 = 1
      | dir == newDir = succ cnt
      | otherwise = 1

title :: String
title = "--- Day 17: Clumsy Crucible ---"

solve :: String -> String
solve s = mconcat ["\n", header, "Part 1: ", part1, "\nPart 2: ", part2, "\n"]
  where
    input = parseInput s
    header = makeTitle title
    part1 = show $ solve1 input
    part2 = show $ solve2 input

solve1 :: Grid -> Int
solve1 = minimumHeatLoss 0 3

solve2 :: Grid -> Int
solve2 = minimumHeatLoss 4 10

minimumHeatLoss :: Int -> Int -> Grid -> Int
minimumHeatLoss minstep maxstep grid = fst . fromJust . aStar nextStates cost estimate isTarget $ start
  where
    isInGrid st = row `elem` [1 .. nrows grid] && col `elem` [1 .. ncols grid]
      where
        (row, col) = position st
    dist (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)
    end = (nrows grid, ncols grid)
    nextStates = getNextStates minstep maxstep `pruning` (not . isInGrid)
    cost _ st = grid ! position st
    estimate = dist end . position
    isTarget st = position st == end && stepCount st >= minstep
    start = State (1, 1) East 0

getNextStates :: Int -> Int -> State -> [State]
getNextStates minstep maxstep st
  | mustKeepGoing = filter (\d -> direction d == dir) possibleDirections
  | otherwise = possibleDirections
  where
    dir = direction st
    mustKeepGoing = stepCount st `elem` [1 .. pred minstep]
    possibleDirections =
      filter (\s -> stepCount s <= maxstep) $
        filter (\s -> position s /= (1, 1)) $
          map (move st) $
            filter (\d -> d /= opposite dir) allDirections

parseInput :: String -> Grid
parseInput = fromLists . map (map digitToInt) . lines

example :: IO String
example = readFile "examples/ex17.txt"
