-- usage: cabal run aoc23 03
-- description:

module Day03 where

import AOCUtils.Input (makeTitle)

title :: String
title = "--- Day 4: Scratchcards ---"

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