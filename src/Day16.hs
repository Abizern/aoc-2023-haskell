-- usage: cabal run aoc23 16
-- description: Tracing a path through a grid.
-- I used Data.Matrix and a Breadth First Search
--
-- Remember - a Matrix is 1-indexed

module Day16 where

import AOCUtils.Input (makeTitle)
import Data.Matrix (Matrix, fromLists, ncols, nrows, (!))
import Data.Set (Set)
import qualified Data.Set as Set (empty, insert, map, member, size)

title :: String
title = "--- Day 16: The Floor Will Be Lava ---"

type Grid = Matrix Char

data Direction = U | R | D | L deriving (Eq, Show, Ord)

solve :: String -> String
solve s = mconcat ["\n", header, "Part 1: ", part1, "\nPart 2: ", part2, "\n"]
  where
    input = parseInput s
    header = makeTitle title
    part1 = show $ solve1 input
    part2 = show $ solve2 input

solve1 :: Grid -> Int
solve1 g = traceFlow Set.empty g [Flow 1 1 R]

solve2 :: Grid -> Int
solve2 g = maximum [x | start <- (candidateFlows g), let x = traceFlow Set.empty g [start]]

parseInput :: String -> Grid
parseInput = fromLists . lines

data Flow = Flow Int Int Direction deriving (Eq, Show, Ord)

makeFlow :: (Int, Int, Direction) -> Flow
makeFlow (r, c, d) = Flow r c d

tile :: Flow -> (Int, Int)
tile (Flow a b _) = (a, b)

nextFlows :: Grid -> Flow -> [Flow]
nextFlows grid flow =
  let reflector (Flow a b _) = grid ! (a, b) -- I'm checking for valid flows at creation
      validFlows (Flow a b _) = a `elem` [1 .. nrows grid] && b `elem` [1 .. ncols grid]
      nextFlows' ref (Flow rr cc U)
        | ref `elem` ".|" = [Flow (rr - 1) cc U]
        | ref == '/' = [Flow rr (cc + 1) R]
        | ref == '\\' = [Flow rr (cc - 1) L]
        | ref == '-' = [Flow rr (cc - 1) L, Flow rr (cc + 1) R]
      nextFlows' ref (Flow rr cc R)
        | ref `elem` ".-" = [Flow rr (cc + 1) R]
        | ref == '/' = [Flow (rr - 1) cc U]
        | ref == '\\' = [Flow (rr + 1) cc D]
        | ref == '|' = [Flow (rr - 1) cc U, Flow (rr + 1) cc D]
      nextFlows' ref (Flow rr cc D)
        | ref `elem` ".|" = [Flow (rr + 1) cc D]
        | ref == '/' = [Flow rr (cc - 1) L]
        | ref == '\\' = [Flow rr (cc + 1) R]
        | ref == '-' = [Flow rr (cc - 1) L, Flow rr (cc + 1) R]
      nextFlows' ref (Flow rr cc L)
        | ref `elem` ".-" = [Flow rr (cc - 1) L]
        | ref == '/' = [Flow (rr + 1) cc D]
        | ref == '\\' = [Flow (rr - 1) cc U]
        | ref == '|' = [Flow (rr + 1) cc D, Flow (rr - 1) cc U]
      nextFlows' _ _ = error "Malformed inputs"
   in filter validFlows $ nextFlows' (reflector flow) flow

traceFlow :: Set Flow -> Grid -> [Flow] -> Int
traceFlow s g fs = case (s, g, fs) of
  (seen, _, []) -> Set.size . Set.map tile $ seen
  (seen, grid, x : xs)
    | x `Set.member` seen -> traceFlow seen grid xs
    | otherwise -> traceFlow (Set.insert x seen) grid (xs ++ newFlows)
    where
      newFlows = nextFlows grid x

candidateFlows :: Grid -> [Flow]
candidateFlows g = map makeFlow $ rights ++ lefts ++ downs ++ ups
  where
    rights = zip3 [1 .. nrows g] (repeat (1 :: Int)) (repeat R)
    lefts = zip3 [1 .. nrows g] (repeat $ ncols g) (repeat L)
    downs = zip3 (repeat (1 :: Int)) [1 .. ncols g] (repeat D)
    ups = zip3 (repeat $ nrows g) [1 .. ncols g] (repeat U)

example :: IO String
example = readFile "examples/ex16.txt"
