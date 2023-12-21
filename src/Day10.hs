-- usage: cabal run aoc23 10
-- description:

module Day10 where

import AOCUtils.Input (makeTitle)
import Data.Matrix (Matrix, fromLists, ncols, nrows, (!))

title :: String
title = "--- Day 10: Pipe Maze ---"

type Grid = Matrix Char

data Direction = North | East | South | West deriving (Eq, Show, Ord)

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
    input = s
    header = makeTitle title
    part1 = show $ solve1 input
    part2 = show $ solve2 input

solve1 :: String -> String
solve1 = id

solve2 :: String -> String
solve2 = id

example1 :: IO String
example1 = readFile "examples/ex10_1.txt"

example2 :: IO String
example2 = readFile "examples/ex10_2.txt"
