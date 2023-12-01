-- --- Day 1: Trebuchet?! ---
-- usage: runghc Day01.hs < ../input/day01.txt
--
-- description: Find digits in a string

module Day01 where

import Data.Char (isDigit)
import Data.List (isPrefixOf, tails)
import qualified Data.List.NonEmpty as NE (fromList, head, last)
import Data.Maybe (mapMaybe)

firstDigitIn :: String -> Char
firstDigitIn s = NE.head $ NE.fromList $ take 1 $ [c | c <- s, isDigit c]

firstAndLast :: [Char] -> Int
firstAndLast s = read [firstDigit, lastDigit] :: Int
  where
    firstDigit = firstDigitIn s
    lastDigit = firstDigitIn $ reverse s

solve1 :: [String] -> Int
solve1 s = sum $ map firstAndLast s

extractNumber :: String -> [Char]
extractNumber _ = ['1']

convert :: [Char] -> Maybe Char
convert [] = Nothing
convert s@(x : _)
  | "one" `isPrefixOf` s = Just '1'
  | "two" `isPrefixOf` s = Just '2'
  | "three" `isPrefixOf` s = Just '3'
  | "four" `isPrefixOf` s = Just '4'
  | "five" `isPrefixOf` s = Just '5'
  | "six" `isPrefixOf` s = Just '6'
  | "seven" `isPrefixOf` s = Just '7'
  | "eight" `isPrefixOf` s = Just '8'
  | "nine" `isPrefixOf` s = Just '9'
  | isDigit x = Just x
  | otherwise = Nothing

firstAndLast' :: [Char] -> Int
firstAndLast' s = read [firstDigit, lastDigit] :: Int
  where
    digits = NE.fromList $ mapMaybe convert $ tails s
    firstDigit = NE.head digits
    lastDigit = NE.last digits

solve2 :: [String] -> Int
solve2 s = sum $ map firstAndLast' s

solve :: String -> String
solve s = mconcat [part1, "\n", part2, "\n"]
  where
    input = lines s
    part1 = show $ sum $ map firstAndLast input
    part2 = show $ sum $ map firstAndLast' input
