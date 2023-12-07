-- usage: cabal run aoc23 7
-- description: Writing custom orderings for data structures.

module Day07 where

import AOCUtils.Input (makeTitle)
import AOCUtils.Lists (replace)
import AOCUtils.Strings (extractNumber)
import Data.List (group, sort, sortBy, sortOn)

title :: String
title = "--- Day 7: Camel Cards ---"

solve :: String -> String
solve s = mconcat [header, "Part 1: ", part1, "\nPart 2: ", part2, "\n"]
  where
    inp = parseInput s
    inp2 = parseInputWithJokers s
    header = makeTitle title
    part1 = show $ solve1 inp
    part2 = show $ solve2 inp2

solve1 :: [(Hand, Integer)] -> Integer
solve1 inp = result
  where
    ordered = sortOn fst inp
    indexed = zip [1 :: Integer ..] $ map snd ordered
    result = sum $ map (uncurry (*)) indexed

solve2 :: [(Hand, Integer)] -> Integer
solve2 inp = result
  where
    ordered = sortOn fst inp
    indexed = zip [1 :: Integer ..] $ map snd ordered
    result = sum $ map (uncurry (*)) indexed

data HandType
  = High
  | Pair1
  | Pair2
  | Threes
  | Full
  | Fours
  | Fives
  deriving (Eq, Show, Ord)

data Card
  = Joker
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | J
  | Q
  | K
  | A
  deriving (Eq, Ord, Show)

newtype Hand = Hand [Card] deriving (Eq, Show)

handType :: Hand -> HandType
handType (Hand cards) = grouping
  where
    cards' = filter (/= Joker) cards
    numJokers = 5 - length cards'
    grouping = case sortBy (flip compare) $ map length $ group $ sort cards' of
      [] -> Fives
      [x] | x == 5 - numJokers -> Fives
      [x, 1] | x == 4 - numJokers -> Fours
      [x, 2] | x == 3 - numJokers -> Full
      [x, 1, 1] | x == 3 - numJokers -> Threes
      [x, 2, 1] | x == 2 - numJokers -> Pair2
      [x, 1, 1, 1] | x == 2 - numJokers -> Pair1
      _ -> High

highCompare :: Hand -> Hand -> Ordering
highCompare (Hand cards1) (Hand cards2) = result
  where
    result = findFirst $ zip cards1 cards2
    findFirst [] = EQ
    findFirst ((a, b) : xs)
      | a == b = findFirst xs
      | otherwise = compare a b

instance Ord Hand where
  compare h1 h2
    | handType h1 == handType h2 = highCompare h1 h2
    | otherwise = compare (handType h1) (handType h2)

parseCard :: Char -> Card
parseCard c = case c of
  'Z' -> Joker
  '2' -> Two
  '3' -> Three
  '4' -> Four
  '5' -> Five
  '6' -> Six
  '7' -> Seven
  '8' -> Eight
  '9' -> Nine
  'T' -> Ten
  'J' -> J
  'Q' -> Q
  'K' -> K
  'A' -> A
  _ -> error "unknown entry"

makeHand :: String -> Hand
makeHand s = Hand cards
  where
    cards = map parseCard s

parseInput :: String -> [(Hand, Integer)]
parseInput s = map extractor $ lines s
  where
    extractor str = case words str of
      [a, b] -> (makeHand a, fromIntegral $ extractNumber b)
      _ -> error "incompatible input"

parseInputWithJokers :: String -> [(Hand, Integer)]
parseInputWithJokers s = map extractor $ lines ss
  where
    ss = replace 'J' 'Z' s
    extractor str = case words str of
      [a, b] -> (makeHand a, fromIntegral $ extractNumber b)
      _ -> error "incompatible input"
