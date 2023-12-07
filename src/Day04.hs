-- usage: cabal run aoc23 04
-- description: Read winning lottery numbers and count generations without excessive iteration

module Day04 where

import AOCUtils.Input (makeTitle)
import AOCUtils.Strings (extractNumber)
import qualified Data.List.NonEmpty as NE (fromList, head, last)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map (elems, empty, insert, lookup)

title :: String
title = "--- Day 4: Scratchcards ---"

solve :: String -> String
solve s = mconcat ["\n", header, "Part 1: ", part1, "\nPart 2: ", part2, "\n"]
  where
    cards = map readCard $ lines s
    header = makeTitle title
    part1 = show $ solve1 cards
    part2 = show $ solve2 cards

solve1 :: [Card] -> Int
solve1 = sum . map score

solve2 :: [Card] -> Int
solve2 = sum . Map.elems . foldr accumulator Map.empty

readNumbers :: String -> [Int]
readNumbers s = map read $ filter (/= "") $ splitOn " " s

data Card = Card Int [Int] [Int] deriving (Eq, Show)

readCard :: String -> Card
readCard s = Card nr targets values
  where
    firstSplit = NE.fromList $ splitOn ":" s
    nr = extractNumber $ NE.head firstSplit
    secondSplit = splitOn "|" $ NE.last firstSplit
    lists = NE.fromList $ map readNumbers secondSplit
    targets = NE.head lists
    values = NE.last lists

score :: Card -> Int
score card = result
  where
    m = matches card
    result = if m == 0 then 0 else 2 ^ (m - 1)

matches :: Card -> Int
matches (Card _ ts vs) = sum [1 | t <- ts, v <- vs, t == v]

incrementCounts :: [Int] -> Map Int Int -> Map Int Int
incrementCounts l m = incrementCounts' (reverse l) m
  where
    incrementCounts' [] m' = m'
    incrementCounts' (x : xs) m' = incrementCounts xs $ Map.insert x newValue m
      where
        oldValue = case Map.lookup x m' of
          Just n -> n
          _ -> 0
        newValue = if oldValue == 0 then 1 else oldValue + 1

accumulator :: Card -> Map Int Int -> Map Int Int
accumulator c@(Card n _ _) m = Map.insert n value m
  where
    s = matches c
    value = succ $ sum $ take s $ Map.elems m
