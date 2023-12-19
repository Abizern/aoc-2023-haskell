-- usage: cabal run aoc23 5
-- description: Create functions on ranges.

module Day05 where

import AOCUtils.Input (makeTitle, newlines, spacedNumbers)
import Data.Range

title :: String
title = "--- Day 5: If You Give A Seed A Fertilizer ---"

type RangeMap = (Range Int, Int) -- The Range (+=*) and the offset to apply

type Input = ([Int], [[RangeMap]])

solve :: String -> String
solve s = mconcat ["\n", header, "Part 1: ", part1, "\nPart 2: ", part2, "\n"]
  where
    input = parseInput s
    header = makeTitle title
    part1 = show $ solve1 input
    part2 = show $ solve2 input

solve1 :: Input -> Int
solve1 (inputs, ranges) =
  minimum
    [ v
      | i <- inputs,
        let v = processMappings i ranges
    ]

solve2 :: Input -> Int
solve2 _ = 2

processMappings :: Int -> [[RangeMap]] -> Int
processMappings = foldl process
  where
    process i [] = i
    process i (x : xs) = if inRange (fst x) i then i + snd x else process i xs

-- Parsing Functions

parseInput :: String -> Input
parseInput = blocks . newlines
  where
    blocks (x : xs) = (parseSeeds x, map parseRangeMaps xs)
    blocks _ = error "Malformed input"

parseSeeds :: String -> [Int]
parseSeeds = spacedNumbers . drop (length "seeds:")

parseRangeMaps :: String -> [RangeMap]
parseRangeMaps = map parseRangeMap . drop 1 . lines

parseRangeMap :: String -> RangeMap
parseRangeMap = parseRangeMap' . spacedNumbers
  where
    parseRangeMap' args = case args of
      [d, s, l] -> (s +=* (s + l), d - s)
      _ -> error "Malformed input"

example :: IO String
example = readFile "examples/ex05.txt"
