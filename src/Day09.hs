-- usage: cabal run aoc23 9
-- description:

module Day09 where

import AOCUtils.Input (makeTitle, spacedNumbers)

title :: String
title = "--- Day 9: Mirage Maintenance ---"

solve :: String -> String
solve s = mconcat [header, "Part 1: ", part1, "\nPart 2: ", part2, "\n"]
  where
    input = parseInput s
    header = makeTitle title
    part1 = show $ solve1 input
    part2 = show $ solve2 input

solve1 :: [[Int]] -> Int
solve1 = sum . map nextNumber

solve2 :: [[Int]] -> Int
solve2 = sum . map previousNumber

nextNumber :: [Int] -> Int
nextNumber nums = case (reverse nums, listOfDifferences nums) of
  (x : _, diffs@(y : _))
    | areAllSame diffs -> x + y
    | otherwise -> x + nextNumber diffs
  _ -> error "We've run out of numbers"

previousNumber :: [Int] -> Int
previousNumber nums = case (nums, listOfDifferences nums) of
  (x : _, diffs@(y : _))
    | areAllSame diffs -> x - y
    | otherwise -> x - previousNumber diffs
  _ -> error "We've run out of numbers"

listOfDifferences :: [Int] -> [Int]
listOfDifferences xs = zipWith (flip (-)) xs (drop 1 xs)

areAllSame :: (Eq a) => [a] -> Bool
areAllSame [] = True
areAllSame (x : xs) = all (== x) xs

parseInput :: String -> [[Int]]
parseInput s = map spacedNumbers $ lines s
