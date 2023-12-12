-- usage: cabal run aoc23 12
-- description: Recursively find solutions, with a cache.

module Day12 where

import AOCUtils.Input (makeTitle)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.MemoTrie (memo2)

title :: String
title = "--- Day 12: Hot Springs ---"

solve :: String -> String
solve s = mconcat ["\n", header, "Part 1: ", part1, "\nPart 2: ", part2, "\n"]
  where
    input = parseInput s
    header = makeTitle title
    part1 = show $ solve1 input
    part2 = show $ solve2 input

solve1 :: [(String, [Int])] -> Int
solve1 = sum . map (uncurry count)

solve2 :: [(String, [Int])] -> Int
solve2 = sum . map (uncurry count . expandInput)

count :: String -> [Int] -> Int
count = memo2 count'
  where
    count' springs numbers = case (springs, numbers) of
      (xs, [])
        | all (`elem` ".?") xs -> 1
        | otherwise -> 0
      ([], _) -> 0
      ('.' : xs, nums) -> count xs nums
      ('#' : xs, n : ns) -> case splitAt (n - 1) xs of
        (a, x : b) | length a == (n - 1) && all (`elem` "#?") a && x `elem` "?." -> count b ns
        (a, []) | length a == (n - 1) && all (`elem` "#?") a -> count [] ns
        _ -> 0
      ('?' : xs, nums) -> count ('.' : xs) nums + count ('#' : xs) nums
      (xs, xn) -> error (show (xs, xn))

parseInput :: String -> [(String, [Int])]
parseInput s =
  [ (springs, p)
    | [springs, raw] <- map words (lines s),
      let p = map read (splitOn "," raw)
  ]

expandInput :: (String, [Int]) -> (String, [Int])
expandInput (s, n) = (expandSprings s, expandNumbers n)

expandSprings :: String -> String
expandSprings = intercalate "?" . replicate 5

expandNumbers :: [Int] -> [Int]
expandNumbers = concat . replicate 5

example :: IO String
example = readFile "examples/ex12.txt"
