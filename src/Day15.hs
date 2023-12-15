-- usage: cabal run aoc23 15
-- description:

module Day15 where

import AOCUtils.Input (makeTitle)
import Data.Char (ord)
import Data.List (foldl')
import Data.List.Split (splitOn)

title :: String
title = "--- Day 15: Lens Library ---"

solve :: String -> String
solve s = mconcat ["\n", header, "Part 1: ", part1, "\nPart 2: ", part2, "\n"]
  where
    input = parseInput s
    header = makeTitle title
    part1 = show $ solve1 input
    part2 = show $ solve2 input

solve1 :: [String] -> Int
solve1 = sum . map hasher

solve2 :: [String] -> Int
solve2 _ = 2

hasher :: String -> Int
hasher = foldl' hasher' 0
  where
    hasher' accum ch = 17 * (accum + ord ch) `mod` 256

parseInput :: String -> [String]
parseInput = splitOn "," . takeWhile (/= '\n')

example :: IO String
example = readFile "examples/ex15.txt"
