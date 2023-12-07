-- usage: cabal run aoc23 5
-- description: Create functions on ranges.

module Day05 where

import AOCUtils.Input (makeTitle, newlines)

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

-- These functions are just here to make working with the REPL easier.

rawExample :: IO String
rawExample = readFile "examples/ex05.txt"

parseInput s = raw
  where
    raw = lines s