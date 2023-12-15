-- usage: cabal run aoc23 14
-- description: Rotate grids

module Day14 where

import AOCUtils.Input (makeTitle)
import Data.List (elemIndex, transpose)
import Data.Map (Map)
import qualified Data.Map as Map (empty, lookup)

title :: String
title = "--- Day 14: Parabolic Reflector Dish ---"

solve :: String -> String
solve s = mconcat ["\n", header, "Part 1: ", part1, "\nPart 2: ", part2, "\n"]
  where
    input = lines s
    header = makeTitle title
    part1 = show $ solve1 input
    part2 = show $ solve2 input

solve1 :: Grid -> Int
solve1 = load . north

solve2 :: Grid -> Int
solve2 _ = 2

type Grid = [String]

-- | Move all the rocks that can move to the left in each row.
roll :: String -> String
roll = roll' (0 :: Int)
  where
    roll' n ('.' : gs) = roll' (n + 1) gs
    roll' n ('O' : gs) = '0' : roll' n gs
    roll' n ('#' : gs) = replicate n '.' ++ '#' : roll' 0 gs
    roll' n _ = replicate n '.'

west :: Grid -> Grid
west = map roll

east :: Grid -> Grid
east = map (reverse . roll . reverse)

north :: Grid -> Grid
north = transpose . west . transpose

south :: Grid -> Grid
south = transpose . east . transpose

load :: Grid -> Int
load g = sum $ zipWith (*) [1 :: Int ..] (reverse rocks)
  where
    rocks = map (length . filter (== '0')) g

repeatIndices cs = repeatIndices' Map.empty Grid (0 :: Int) cs
  where
    repeatIndices' = undefined

example :: IO String
example = readFile "examples/ex14.txt"
