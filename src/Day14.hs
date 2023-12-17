-- usage: cabal run aoc23 14
-- description: Rotate grids finding and optimise evolving states with

module Day14 where

import AOCUtils.Input (makeTitle)
import Data.List (elemIndex, transpose)
import Data.Set (Set)
import qualified Data.Set as Set (empty, insert, member)

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
solve2 g = load finalState
  where
    (start, grids) = platformStates g
    cycleLength = length grids - start
    cycleIndex = ((1000000000 - start) `rem` cycleLength) + start
    finalState = grids !! cycleIndex

type Grid = [String]

-- | Move all the rocks that can move to the left in each row.
roll :: String -> String
roll = roll' (0 :: Int)
  where
    roll' n ('.' : gs) = roll' (n + 1) gs
    roll' n ('O' : gs) = 'O' : roll' n gs
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
    rocks = map (length . filter (== 'O')) g

cyclePlatform :: Grid -> Grid
cyclePlatform = east . south . west . north

platformStates :: Grid -> (Int, [Grid])
platformStates = platformStates' Set.empty []

platformStates' :: Set Grid -> [Grid] -> Grid -> (Int, [Grid])
platformStates' set states grid = case (set, states, grid) of
  (s, gs, g)
    | Set.member g s -> processStates gs g
    | otherwise -> platformStates' (Set.insert g s) (g : gs) (cyclePlatform g)

processStates :: [Grid] -> Grid -> (Int, [Grid])
processStates gs g = (start, grids)
  where
    grids = reverse gs
    start = case elemIndex g grids of
      Just x -> x
      _ -> error "The state really should exist"

example :: IO String
example = readFile "examples/ex14.txt"
