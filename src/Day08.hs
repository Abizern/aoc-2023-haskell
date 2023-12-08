-- usage: cabal run aoc23 08
-- description:

module Day08 where

import AOCUtils.Input (makeTitle)
import Data.Map (Map)
import qualified Data.Map as Map (fromList, lookup)

title :: String
title = "--- Add day title here ---"

solve :: String -> String
solve s = mconcat [header, "Part 1: ", part1, "\nPart 2: ", part2, "\n"]
  where
    input = parseInput s
    header = makeTitle title
    part1 = show $ solve1 input
    part2 = show $ solve2 input

solve1 :: (String, Map String (String, String)) -> Int
solve1 = runMapping

solve2 :: (String, Map String (String, String)) -> Int
solve2 _ = 2

runMapping :: (String, Map String (String, String)) -> Int
runMapping (instructions, mapping) = length $ run ["AAA"] (cycle instructions)
  where
    run accum ins = case (accum, ins) of
      (acc@(a : as), i : is)
        | a == "ZZZ" -> as
        | i == 'L' -> run (fst value : acc) is
        | i == 'R' -> run (snd value : acc) is
        | otherwise -> as
        where
          value = case Map.lookup a mapping of
            Just x -> x
            Nothing -> error "Missing value in dictionary"
      _ -> accum

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
