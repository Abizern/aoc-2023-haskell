-- usage: cabal run aoc23 13
-- description: Find matching lines in a grid

module Day13 where

import AOCUtils.Input (makeTitle, newlines)
import Data.List (inits, tails, transpose)

title :: String
title = "--- Day 13: Point of Incidence ---"

solve :: String -> String
solve s = mconcat ["\n", header, "Part 1: ", part1, "\nPart 2: ", part2, "\n"]
  where
    input = parseInput s
    header = makeTitle title
    part1 = show $ solve1 input
    part2 = show $ solve2 input

solve1 :: [[String]] -> Int
solve1 = sum . concatMap summarise

solve2 :: [[String]] -> Int
solve2 = sum . concatMap summariseSmudge

reflectionLine :: [String] -> [Int]
reflectionLine grid =
  [ i
    | (i, j, k) <- zip3 [0 :: Int ..] (inits grid) (tails grid),
      not (null j),
      not (null k),
      let comparison x y = if x == y then 0 else 1,
      let differences x y = sum (zipWith comparison x y),
      (0 :: Int) == sum (zipWith differences (reverse j) k)
  ]

smudgeLine :: [String] -> [Int]
smudgeLine grid =
  [ i
    | (i, j, k) <- zip3 [0 :: Int ..] (inits grid) (tails grid),
      not (null j),
      not (null k),
      let comparison x y = if x == y then 0 else 1,
      let differences x y = sum (zipWith comparison x y),
      (1 :: Int) == sum (zipWith differences (reverse j) k)
  ]

summarise :: [String] -> [Int]
summarise grids = map (100 *) (reflectionLine grids) ++ reflectionLine (transpose grids)

summariseSmudge :: [String] -> [Int]
summariseSmudge grids = map (100 *) (smudgeLine grids) ++ smudgeLine (transpose grids)

parseInput :: String -> [[String]]
parseInput = map lines . newlines

example :: IO String
example = readFile "examples/ex13.txt"
