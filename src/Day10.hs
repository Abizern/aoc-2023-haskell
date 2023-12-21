-- usage: cabal run aoc23 10
-- description: Follow a path through a grid.
-- I got carried away with using BFS, but I wanted an excuse to try out the Algorithm.Search package.
-- Part 2 uses Shoelace formula and Pick's theorem, the same way as Day18

module Day10 where

import AOCUtils.Input (makeTitle)
import Algorithm.Search (bfs)
import Data.Matrix (Matrix (..), fromLists, ncols, nrows, (!))

title :: String
title = "--- Day 10: Pipe Maze ---"

type Grid = Matrix Char

data Direction = North | East | South | West deriving (Eq, Show, Ord)

opposite :: Direction -> Direction
opposite North = South
opposite East = West
opposite South = West
opposite West = East

connectors :: Char -> [Direction]
connectors c = case c of
  '|' -> [North, South]
  '-' -> [East, West]
  'L' -> [North, East]
  'J' -> [North, West]
  '7' -> [South, West]
  'F' -> [South, East]
  'S' -> [North, East, South, West]
  _ -> []

solve :: String -> String
solve s = mconcat ["\n", header, "Part 1: ", part1, "\nPart 2: ", part2, "\n"]
  where
    input = parseInput s
    header = makeTitle title
    part1 = show $ solve1 input
    part2 = show $ solve2 input

solve1 :: Grid -> Int
solve1 g = length (perimeter g) `div` 2

solve2 :: Grid -> Int
solve2 = internalArea . perimeter

perimeter :: Grid -> [(Int, Int)]
perimeter g = fst state : points
  where
    start = getStart g
    state = getFirstPipe g start
    path = case bfs (getNextPipe g) (\(p, _) -> p == start) state of
      Just list -> list
      _ -> error "No path"
    points = map fst path

shoelaceArea :: [(Int, Int)] -> Int
shoelaceArea ps = abs . (`div` 2) $ sum (zipWith det ps (drop 1 $ cycle ps))
  where
    det (r1, c1) (r2, c2) = r1 * c2 - r2 * c1

internalArea :: [(Int, Int)] -> Int
internalArea ps = sArea - (p `div` 2) + 1
  where
    sArea = shoelaceArea ps
    p = length ps

-- Find the position of the Start point
getStart :: Grid -> (Int, Int)
getStart grid = case search grid of
  [start] -> start
  _ -> error "Can't find start position"
  where
    search g =
      [ (i, j)
        | i <- [1 .. nrows g],
          j <- [1 .. ncols g],
          g ! (i, j) == 'S'
      ]

-- Starting point for searches
-- The grid -> the current coordinated -> (coordinate, entry direction) of first possible exit
-- If the next pipe is North of the start position, then Direction is South
getFirstPipe :: Grid -> (Int, Int) -> ((Int, Int), Direction)
getFirstPipe g source = candidate
  where
    candidates =
      filter
        (candidateFilter g)
        [ ((row, col), dir)
          | ((row, col), dir) <- map (\d -> (nextPipe source d, opposite d)) [North, East, South, West],
            inGrid g (row, col)
        ]
    candidate = case take 1 candidates of
      [x] -> x
      _ -> error "Malformed input"

candidateFilter :: Grid -> ((Int, Int), Direction) -> Bool
candidateFilter g (target, dir) = case (g ! target, dir) of
  (char, d)
    | d == North && char `elem` "|LJ" -> True
    | d == East && char `elem` "-LF" -> True
    | d == South && char `elem` "|7F" -> True
    | d == West && char `elem` "-J7" -> True
    | otherwise -> False

getNextPipe :: Grid -> ((Int, Int), Direction) -> [((Int, Int), Direction)]
getNextPipe g (source, dir) = case (g ! source, dir) of
  ('|', South) -> [(nextPipe source North, South)]
  ('|', _) -> [(nextPipe source South, North)]
  ('-', East) -> [(nextPipe source West, East)]
  ('-', _) -> [(nextPipe source East, West)]
  ('L', North) -> [(nextPipe source East, West)]
  ('L', _) -> [(nextPipe source North, South)]
  ('J', North) -> [(nextPipe source West, East)]
  ('J', _) -> [(nextPipe source North, South)]
  ('7', West) -> [(nextPipe source South, North)]
  ('7', _) -> [(nextPipe source West, East)]
  ('F', South) -> [(nextPipe source East, West)]
  ('F', _) -> [(nextPipe source South, North)]
  (a, b) -> error $ mconcat ["unhandled case ", show a, " ", show b]

-- No bounds checking. Assuming a well constructed pipe maze.
nextPipe :: (Int, Int) -> Direction -> (Int, Int)
nextPipe (row, col) North = (pred row, col)
nextPipe (row, col) East = (row, succ col)
nextPipe (row, col) South = (succ row, col)
nextPipe (row, col) West = (row, pred col)

inGrid :: Grid -> (Int, Int) -> Bool
inGrid g (row, col) = row `elem` [1 .. nrows g] && col `elem` [1 .. ncols g]

-- Input handing

parseInput :: String -> Grid
parseInput s = fromLists $ lines s

example1 :: IO String
example1 = readFile "examples/ex10_1.txt"

example2 :: IO String
example2 = readFile "examples/ex10_2.txt"
