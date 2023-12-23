-- usage: cabal run aoc23 22
-- description:

module Day22 where

import AOCUtils (extractNumber, makeTitle)
import Data.List.Split (splitOn)

title :: String
title = "--- Day 22: Sand Slabs ---"

data Brick = Brick (Int, Int, Int) (Int, Int, Int) deriving (Show, Eq)

makeBrick :: [[Int]] -> Brick
makeBrick [[x1, y1, z1], [x2, y2, z2]] = Brick (x1, y1, z1) (x2, y2, z2)
makeBrick _ = error "Malformed input"

solve :: String -> String
solve s = mconcat [header, "Part 1: ", part1, "\nPart 2: ", part2, "\n"]
  where
    input = s
    header = makeTitle title
    part1 = show $ solve1 input
    part2 = show $ solve2 input

solve1 :: String -> String
solve1 = id

solve2 :: String -> String
solve2 = id

readInt :: String -> Int
readInt = read

parseInput :: String -> [Brick]
parseInput =
  map makeBrick
    . map (map (map extractNumber) . map (splitOn ",") . splitOn "~")
    . lines

example :: IO String
example = readFile "examples/ex22.txt"
