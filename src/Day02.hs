-- usage: cabal run aoc23 02
--
-- description:

module Day02 where

import AOCUtils.Input (makeTitle)
import AOCUtils.Strings (extractNumber)
import Data.List (isSuffixOf)
import qualified Data.List.NonEmpty as NE (fromList, head, last)
import Data.List.Split (splitOn)

title :: String
title = "--- Day 2: Cube Conundrum ---"

solve :: String -> String
solve s = mconcat [header, "Part 1: ", part1, "\nPart 2: ", part2, "\n"]
  where
    input = map parseGame $ lines s
    header = makeTitle title
    part1 = show $ solve1 input
    part2 = show $ solve2 input

solve1 :: [Game] -> Int
solve1 games = sum $ map number validGames
  where
    validGames = filter isValidGame games

solve2 :: [Game] -> Int
solve2 games = sum $ map gamePower games

data Cube = Red | Blue | Green deriving (Eq, Show)

data GameRound = GameRound {red :: Int, green :: Int, blue :: Int} deriving (Eq, Show)

instance Semigroup GameRound where
  GameRound r1 g1 b1 <> GameRound r2 g2 b2 = GameRound (r1 + r2) (g1 + g2) (b1 + b2)

instance Monoid GameRound where
  mempty = GameRound 0 0 0

data Game = Game Int [GameRound] deriving (Eq, Show)

number :: Game -> Int
number (Game n _) = n

-- | >>> parseGameNr "Game 1"
-- 1
parseGameNr :: String -> Int
parseGameNr = extractNumber

-- | >>> parsePartialGameRound "3 blue"
-- GameRound {red = 0, green = 0, blue = 3}
parsePartialGameRound :: String -> GameRound
parsePartialGameRound s
  | "red" `isSuffixOf` s = GameRound n 0 0
  | "green" `isSuffixOf` s = GameRound 0 n 0
  | "blue" `isSuffixOf` s = GameRound 0 0 n
  | otherwise = mempty
  where
    n = extractNumber s

--  | >>> parseGameRound " 3 blue, 4 red"
parseGameRound :: String -> GameRound
parseGameRound s = foldr1 mappend rnd
  where
    rnd = map parsePartialGameRound $ splitOn "," s

-- | >>> parseGame "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
-- Game 3 [GameRound {red = 20, green = 8, blue = 6},GameRound {red = 4, green = 13, blue = 5},GameRound {red = 1, green = 5, blue = 0}]
parseGame :: String -> Game
parseGame s = Game gameNr rounds
  where
    firstSplit = NE.fromList $ splitOn ":" s
    gameNr = parseGameNr $ NE.head firstSplit
    rounds = map parseGameRound $ splitOn ";" $ NE.last firstSplit

isValidGameRound :: GameRound -> Bool
isValidGameRound (GameRound r g b) = r <= 12 && g <= 13 && b <= 14

impossibleGameRound :: GameRound -> Bool
impossibleGameRound = not . isValidGameRound

isValidGame :: Game -> Bool
isValidGame (Game _ rounds) = sum [1 :: Int | rnd <- rounds, impossibleGameRound rnd] == 0

maxGameRound :: GameRound -> GameRound -> GameRound
maxGameRound (GameRound r1 g1 b1) (GameRound r2 g2 b2) = GameRound (max r1 r2) (max g1 g2) (max b1 b2)

gamePower :: Game -> Int
gamePower (Game _ rounds) = r * g * b
  where
    (GameRound r g b) = foldr1 maxGameRound rounds
