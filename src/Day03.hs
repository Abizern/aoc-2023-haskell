-- usage: cabal run aoc23 3
-- description: Read numbers out of a grid.

module Day03 where

import AOCUtils.Grids (rcTuples)
import AOCUtils.Input (makeTitle)
import AOCUtils.Strings (extractNumber)
import Data.Char (isDigit)

title :: String
title = "--- Day 3: Gear Ratios ---"

solve :: String -> String
solve s = mconcat ["\n", header, "Part 1: ", part1, "\nPart 2: ", part2, "\n"]
  where
    input = parseInput s
    header = makeTitle title
    part1 = show $ solve1 input
    part2 = show $ solve2 input

solve1 :: ([Symbol], [Number]) -> Int
solve1 = sumAdjacent

solve2 :: ([Symbol], [Number]) -> Int
solve2 = sumGearRatios

sumAdjacent :: ([Symbol], [Number]) -> Int
sumAdjacent (syms, nums) =
  sum
    [ nn
      | s <- syms,
        n <- nums,
        let nn = number n,
        isAdjacent s n
    ]

sumGearRatios :: ([Symbol], [Number]) -> Int
sumGearRatios (syms, nums) =
  sum
    [ gr
      | s <- syms,
        let parts = filter (isAdjacent s) nums,
        length parts == 2,
        let gr = product . map number $ parts
    ]

data RCChar = RCChar
  { rcRow :: Int,
    rcCol :: Int,
    rcc :: Char
  }
  deriving (Eq, Show)

isSymbol :: RCChar -> Bool
isSymbol rcchar = ch /= '.' && not (isDigit ch)
  where
    ch = rcc rcchar

hasDigit :: RCChar -> Bool
hasDigit rcchar = isDigit (rcc rcchar)

makeRCChar :: (Int, Int, Char) -> RCChar
makeRCChar (row, col, char) = RCChar row col char

parseInput :: String -> ([Symbol], [Number])
parseInput string = (symbols, numbers)
  where
    rcchars = map makeRCChar . rcTuples . lines $ string
    symbols = parseSymbols rcchars
    numbers = parseNumbers rcchars

data Symbol = Symbol
  { symRow :: Int,
    symCol :: Int,
    symbol :: Char
  }
  deriving (Eq, Show)

makeSymbol :: RCChar -> Symbol
makeSymbol (RCChar r c sym) = Symbol r c sym

parseSymbols :: [RCChar] -> [Symbol]
parseSymbols rcchars = map makeSymbol $ filter isSymbol rcchars

parseNumbers :: [RCChar] -> [Number]
parseNumbers rcchars = case foldr collectDigits ([], []) . filter hasDigit $ rcchars of
  (accum, nums) -> makeNumber accum : nums

data Number = Number
  { numRow :: Int,
    numStart :: Int,
    numEnd :: Int,
    number :: Int
  }
  deriving (Eq, Show)

makeNumber :: [RCChar] -> Number
makeNumber rcchars = Number row s e d
  where
    row = maximum $ map rcRow rcchars
    cols = map rcCol rcchars
    s = minimum cols
    e = maximum cols
    d = extractNumber $ map rcc rcchars

collectDigits :: RCChar -> ([RCChar], [Number]) -> ([RCChar], [Number])
collectDigits r ([], ns) = ([r], ns)
collectDigits r (accum, ns) = case (r, accum, ns) of
  (RCChar _ col _, x : _, numbers)
    | col == pred (rcCol x) -> (r : accum, numbers)
    | otherwise -> ([r], makeNumber accum : numbers)

example :: IO String
example = readFile "examples/ex03.txt"

isAdjacent :: Symbol -> Number -> Bool
isAdjacent s n = case (s, n) of
  (Symbol sr sc _, Number nr ns ne _) -> sr `elem` [nr - 1 .. nr + 1] && sc `elem` [ns - 1 .. ne + 1]
