-- usage: cabal run aoc23 5
-- description: Create functions on ranges.

module Day05 where

import AOCUtils.Input (makeTitle, newlines)
import Data.List.Split (splitOn)
import Data.Range

title :: String
title = "--- Day 5: If You Give A Seed A Fertilizer ---"

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

parseInput s = undefined

-- These functions are just here to make working with the REPL easier.

example :: IO String
example = readFile "examples/ex05.txt"
