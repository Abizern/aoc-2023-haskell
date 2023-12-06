-- usage: cabal run aoc23 6
-- description: Find the real roots of a quadratic equation and count the range

module Day06 where

import AOCUtils.Input
  ( makeTitle,
    spacedNumbers,
  )
import AOCUtils.Strings (extractNumber)

title :: String
title = "--- Day 6: Wait For It ---"

solve :: String -> String
solve s = mconcat ["\n", header, "Part 1: ", part1, "\nPart 2: ", part2, "\n"]
  where
    header = makeTitle title
    part1 = show . solve1 $ parsePairs s
    part2 = show . solve2 $ parseLarge s

solve1 :: [(Int, Int)] -> Int
solve1 = product . map nrSolutions

solve2 :: (Int, Int) -> Int
solve2 = nrSolutions

parsePairs :: String -> [(Int, Int)]
parsePairs s = result
  where
    parseLine = spacedNumbers . dropWhile (/= ' ')
    ls = map parseLine $ lines s
    result = case ls of
      (x : y : _) -> zip x y
      _ -> error "Incorrect input"

parseLarge :: String -> (Int, Int)
parseLarge s = case lines s of
  (x : y : _) -> (extractNumber x, extractNumber y)
  _ -> error "incorrect input"

nrSolutions :: (Int, Int) -> Int
nrSolutions (b, c) = nr
  where
    rb = fromIntegral b :: Double
    d = sqrt $ fromIntegral $ b * b - 4 * c
    r1 = ceiling $ ((rb + d) / 2.0) - 1
    r2 = floor $ ((rb - d) / 2.0) + 1
    nr = r1 + 1 - r2
