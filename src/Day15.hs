-- usage: cabal run aoc23 15
-- description: Create a hash in part 1, use that as a key for a dictionary which is mutated.

module Day15 where

import AOCUtils.Input (makeTitle)
import Data.Char (ord)
import Data.List (elemIndex, foldl')
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map (empty, findWithDefault, foldrWithKey, insert)

title :: String
title = "--- Day 15: Lens Library ---"

solve :: String -> String
solve s = mconcat ["\n", header, "Part 1: ", part1, "\nPart 2: ", part2, "\n"]
  where
    input = parseInput s
    header = makeTitle title
    part1 = show $ solve1 input
    part2 = show $ solve2 input

solve1 :: [String] -> Int
solve1 = sum . map hasher

solve2 :: [String] -> Int
solve2 l = focussingPower boxes
  where
    instructions = map parseInstruction l
    boxes = foldl' processInstruction Map.empty instructions

hasher :: String -> Int
hasher = foldl' hasher' 0
  where
    hasher' accum ch = 17 * (accum + ord ch) `mod` 256

data Lens = Lens String Int deriving (Show)

-- | This makes it easier to check lenses in a box for removal and replacement
instance Eq Lens where
  Lens s1 _ == Lens s2 _ = s1 == s2

type LBox = [Lens]

type LBoxes = Map Int LBox

data Instruction = Add Lens | Remove Lens deriving (Eq, Show)

focussingPower :: LBoxes -> Int
focussingPower = Map.foldrWithKey focussingPower' 0
  where
    boxPower bs = sum [n | (i, Lens _ f) <- zip [1 :: Int ..] bs, let n = i * f]
    focussingPower' idx b accum = succ idx * boxPower b + accum

parseInstruction :: String -> Instruction
parseInstruction s = case splitOn "=" s of
  [x] -> Remove $ Lens (init x) 0
  [x, y] -> Add $ Lens x (read y)
  _ -> error "Malformed input"

processInstruction :: LBoxes -> Instruction -> LBoxes
processInstruction boxes instruction =
  let idx = box instruction
      lbox = Map.findWithDefault [] idx boxes
      process (Add l) = Map.insert idx (addLens l lbox) boxes
      process (Remove l) = Map.insert idx (removeLens l lbox) boxes
   in process instruction

addLens :: Lens -> LBox -> LBox
addLens l b = case elemIndex l b of
  Just n -> take n b ++ [l] ++ drop (succ n) b
  Nothing -> b ++ [l]

removeLens :: Lens -> LBox -> LBox
removeLens l = filter (/= l)

box :: Instruction -> Int
box i = case i of
  Add (Lens s _) -> hasher s
  Remove (Lens s _) -> hasher s

parseInput :: String -> [String]
parseInput = splitOn "," . takeWhile (/= '\n')

example :: IO String
example = readFile "examples/ex15.txt"
