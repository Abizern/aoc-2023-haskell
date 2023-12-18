-- usage: cabal run aoc23 18
-- description: Compute the Area of a dug out polygon
-- Complicated by the border having a with of 1.
-- Use the Shoelace formula for the total area, which is wrong because it's in the middle of the border.
-- Use this with Pick's Theorem to get the interior area.
-- Get the total area by adding the perimeter to the internal area.

module Day18 where

import AOCUtils.Input (makeTitle)
import Data.List.Split (splitOn)

title :: String
title = "--- Day 18: Lavaduct Lagoon ---"

solve :: String -> String
solve s = mconcat ["\n", header, "Part 1: ", part1, "\nPart 2: ", part2, "\n"]
  where
    input = corners . parseInput $ s
    header = makeTitle title
    part1 = show $ solve1 input
    part2 = show $ solve2 input

solve1 :: [Corner] -> Int
solve1 = totalArea

solve2 :: [Corner] -> Int
solve2 _ = 2

type Corner = (Int, Int)

type Instruction = (String, Int)

parseInput :: String -> [Instruction]
parseInput = map parseInstruction . lines
  where
    parseInstruction s = case splitOn " " s of
      (x : y : _) -> (x, read y)
      _ -> error "malformed input"

corners :: [Instruction] -> [Corner]
corners = scanl dig (0, 0)
  where
    dig (r, c) ("U", x) = (r - x, c)
    dig (r, c) ("R", x) = (r, c + x)
    dig (r, c) ("D", x) = (r + x, c)
    dig (r, c) ("L", x) = (r, c - x)
    dig _ _ = error "malformed input"

perimeter :: [Corner] -> Int
perimeter cs = sum [d | (a, b) <- zip cs (drop 1 cs), let d = dist a b]
  where
    dist (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)

shoelaceArea :: [Corner] -> Int
shoelaceArea cs = abs . (`div` 2) $ sum (zipWith det cs (drop 1 $ cycle cs))
  where
    det (r1, c1) (r2, c2) = r1 * c2 - r2 * c1

internalArea :: [Corner] -> Int
internalArea cs = sArea - (p `div` 2) + 1
  where
    sArea = shoelaceArea cs
    p = perimeter cs

totalArea :: [Corner] -> Int
totalArea cs = p + iArea
  where
    p = perimeter cs
    iArea = internalArea cs

example :: IO String
example = readFile "examples/ex18.txt"
