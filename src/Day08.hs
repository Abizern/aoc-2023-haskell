-- usage: cabal run aoc23 8
-- description: Follow a route, and then find a cycle length. Spot the required optimisation.

module Day08 where

import AOCUtils.Input (makeTitle)
import Data.List (isSuffixOf)
import Data.Map (Map)
import qualified Data.Map.Strict as Map (fromList, keys, lookup)

title :: String
title = "--- Day 8: Haunted Wasteland ---"

solve :: String -> String
solve s = mconcat ["\n", header, "Part 1: ", part1, "\nPart 2: ", part2, "\n"]
  where
    input = parseInput s
    header = makeTitle title
    part1 = show $ solve1 input
    part2 = show $ solve2 input

solve1 :: (String, Map String (String, String)) -> Int
solve1 = runMapping "AAA" "ZZZ"

solve2 :: (String, Map String (String, String)) -> Int
solve2 = runGhostMapping

runMapping :: String -> String -> (String, Map String (String, String)) -> Int
runMapping input term (instructions, mapping) = length $ run [input] (cycle instructions)
  where
    run accum ins = case (accum, ins) of
      (acc@(a : as), i : is)
        | term `isSuffixOf` a -> as
        | i == 'L' -> run (fst value : acc) is
        | i == 'R' -> run (snd value : acc) is
        | otherwise -> as
        where
          value = case Map.lookup a mapping of
            Just x -> x
            Nothing -> error "Missing value in dictionary"
      _ -> accum

runGhostMapping :: (String, Map String (String, String)) -> Int
runGhostMapping args@(_, mapping) = foldl lcm 1 cycleLengths
  where
    startPoints = startingPoints $ Map.keys mapping
    cycleLengths = [l | sp <- startPoints, let l = runMapping sp "Z" args]

startingPoints :: [String] -> [String]
startingPoints = filter ("A" `isSuffixOf`)

parseInput :: String -> (String, Map String (String, String))
parseInput s = (instructions, mapping)
  where
    ss = lines s
    instructions = case ss of
      (x : _) -> x
      _ -> ""
    mapping = Map.fromList . map parseMapLine . drop 2 $ ss

parseMapLine :: String -> (String, (String, String))
parseMapLine s = (key, (left, right))
  where
    key = take 3 s
    left = take 3 . drop 7 $ s
    right = take 3 . drop 12 $ s

example1 :: IO String
example1 = readFile "examples/ex08.txt"

example2 :: IO String
example2 = readFile "examples/ex08_2.txt"

example3 :: IO String
example3 = readFile "examples/ex08_3.txt"
