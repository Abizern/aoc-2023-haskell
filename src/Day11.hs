-- usage: cabal run aoc23 11
-- description:

module Day11 where

import AOCUtils.Grids (rcTuples)
import AOCUtils.Input (makeTitle)
import Data.List (tails, transpose)

title :: String
title = "--- Add day title here ---"

solve :: String -> String
solve s = mconcat [header, "Part 1: ", part1, "\nPart 2: ", part2, "\n"]
  where
    input = parseInput s
    header = makeTitle title
    part1 = show $ solve1 input
    part2 = show $ solve2 input

solve1 :: Universe -> Int
solve1 u = distances $ expand 2 u

solve2 :: Universe -> Int
solve2 u = distances $ expand 1000000 u

data Universe = Universe
  { galaxies :: [(Int, Int)],
    emptyRows :: [Int],
    emptyCols :: [Int]
  }
  deriving
    (Eq, Show)

expand :: Int -> Universe -> [(Int, Int)]
expand e (Universe gs rs cs) = [(r', c') | (r, c) <- gs, let r' = newR r, let c' = newC c]
  where
    newR r = r + (e - 1) * sum [1 | a <- rs, b <- [0 .. r], a == b]
    newC c = c + (e - 1) * sum [1 | a <- cs, b <- [0 .. c], a == b]

dist :: (Int, Int) -> (Int, Int) -> Int
dist (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)

distances :: [(Int, Int)] -> Int
distances gs = sum $ [d | (x : rest) <- tails gs, y <- rest, let d = dist x y]

parseInput :: String -> Universe
parseInput s = Universe gs ers ecs
  where
    inp = lines s
    emptyLines :: [String] -> [Int]
    emptyLines ls = [n | (n, c) <- zip [0 :: Int ..] ls, all (== '.') c]
    ers = emptyLines inp
    ecs = emptyLines $ transpose inp
    gs = map (\(a, b, _) -> (a, b)) . filter (\(_, _, c) -> c == '#') $ rcTuples inp

example :: IO String
example = readFile "examples/ex11.txt"
