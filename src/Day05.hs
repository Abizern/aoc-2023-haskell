-- usage: cabal run aoc23 5
-- description: Create functions on ranges.

module Day05 where

import AOCUtils.Input (makeTitle, newlines, spacedNumbers)
import Data.Range (Bound (Bound), Range (..), inRange, intersection, invert, mergeRanges, (+=*))

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
solve2 (inputs, ranges) = minimum lowestValues
  where
    inputRanges = rangesFromSeeds inputs
    processedRanges = foldl processRanges inputRanges ranges
    lowestValues = map minOfRange processedRanges

-- Given a location and a sequence of intermediate mappings, get the final location.
processMappings :: Int -> [[RangeMap]] -> Int
processMappings = foldl process
  where
    process i [] = i
    process i (x : xs) = if inRange (fst x) i then i + snd x else process i xs

-- A folding function
processRanges :: [Range Int] -> [RangeMap] -> [Range Int]
processRanges = processRanges' []
  where
    processRanges' :: [Range Int] -> [Range Int] -> [RangeMap] -> [Range Int]
    processRanges' acc ranges maps = case (acc, ranges, maps) of
      (accum, rs, []) -> mergeRanges $ accum ++ rs
      (accum, rs, x : xs) -> processRanges' (accum ++ newRanges) remainders xs
        where
          (rng, d) = x
          int = intersection rs [rng]
          inv = invert int
          newRanges = map (offsetRange d) int
          remainders = intersection inv rs

minOfRange :: Range Int -> Int
minOfRange (SingletonRange x) = x
minOfRange (SpanRange (Bound val _) _) = val
minOfRange _ = error "Should not have this type of range"

-- Chunks input set list into ranges.
rangesFromSeeds :: [Int] -> [Range Int]
rangesFromSeeds [] = []
rangesFromSeeds [_] = []
rangesFromSeeds (x : y : xs) = x +=* (x + y) : rangesFromSeeds xs

offsetRange :: Int -> Range Int -> Range Int
offsetRange offset (SingletonRange x) = SingletonRange (x + offset)
offsetRange offset (SpanRange x y) = SpanRange (offsetBound x offset) (offsetBound y offset)
offsetRange offset (LowerBoundRange x) = LowerBoundRange (offsetBound x offset)
offsetRange offset (UpperBoundRange x) = UpperBoundRange (offsetBound x offset)
offsetRange _ InfiniteRange = InfiniteRange

offsetBound :: Bound Int -> Int -> Bound Int
offsetBound (Bound val t) offset = Bound (val + offset) t

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
