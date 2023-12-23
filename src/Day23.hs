-- usage: cabal run aoc23 23
-- description:

module Day23 where

import AOCUtils.Input (makeTitle)
import Algorithm.Search (aStar, dijkstra)
import qualified Data.List.NonEmpty as NE (fromList, head)
import Data.Matrix (Matrix, fromLists, ncols, nrows, setElem, (!))
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set (empty, insert, notMember)

title :: String
title = "--- Day 23: A Long Walk ---"

type Grid = Matrix Char

type Coord = (Int, Int)

data State = State Coord Char (Set Coord) deriving (Eq, Show, Ord)

solve :: String -> String
solve s = mconcat ["\n", header, "Part 1: ", part1, "\nPart 2: ", part2, "\n"]
  where
    input = parseInput s
    header = makeTitle title
    part1 = show $ solve1 input
    part2 = show $ solve2 input

solve1 :: Grid -> Int
solve1 _ = 1

solve2 :: Grid -> Int
solve2 _ = 2

maxNrSteps :: Grid -> State -> Int
maxNrSteps grid = fst . fromJust . aStar nextStates' cost estimate isTarget
  where
    dist (r1, c1) (r2, c2) = negate (abs (r1 - r2) + abs (c1 - c2))
    target = NE.head $ NE.fromList $ [(nrows grid, c) | c <- [1 .. ncols grid], grid ! (nrows grid, c) == '.']
    nextStates' = nextStates grid
    cost (State p1 _ _) (State p2 _ _) = dist p1 p2
    estimate (State p _ _) = dist p target
    isTarget (State p _ _) = p == target

maxRoute :: Grid -> State -> Int
maxRoute grid = fst . fromJust . dijkstra nextStates' cost isTarget
  where
    target = NE.head $ NE.fromList $ [(nrows grid, c) | c <- [1 .. ncols grid], grid ! (nrows grid, c) == '.']
    nextStates' = nextStates grid
    cost _ _ = -1
    isTarget (State p _ _) = p == target

debugSteps :: Grid -> State -> [State]
debugSteps grid = snd . fromJust . aStar nextStates' cost estimate isTarget
  where
    dist (r1, c1) (r2, c2) = negate (abs (r1 - r2) + abs (c1 - c2))
    target = NE.head $ NE.fromList $ [(nrows grid, c) | c <- [1 .. ncols grid], grid ! (nrows grid, c) == '.']
    nextStates' = nextStates grid
    cost (State p1 _ _) (State p2 _ _) = dist p1 p2
    estimate (State p _ _) = dist p target
    isTarget (State p _ _) = p == target

nextStates :: Grid -> State -> [State]
nextStates g (State (row, col) char seen) = newStates
  where
    newSeen = Set.insert (row, col) seen
    isInGrid (r, c) = r `elem` [1 .. nrows g] && c `elem` [1 .. ncols g]
    notSeen coord = Set.notMember coord seen
    notForest (State _ ch _) = ch /= '#'
    neighbourCoords = case char of
      ch
        | ch == '>' -> [(row, succ col)]
        | ch == '<' -> [(row, pred col)]
        | ch == '^' -> [(pred row, col)]
        | ch == 'v' -> [(succ row, col)]
        | ch == '.' -> [(row, succ col), (row, pred col), (succ row, col), (pred row, col)]
        | otherwise -> error "Malformed input"
    possibleCoords = filter notSeen $ filter isInGrid neighbourCoords
    newStates = filter notForest . map (\coord -> State coord (g ! coord) newSeen) $ possibleCoords

initialState :: Grid -> State
initialState g = State initialCoord '.' Set.empty
  where
    initialCoord = NE.head $ NE.fromList $ [(1, c) | c <- [1 .. ncols g], g ! (1, c) == '.']

parseInput :: String -> Grid
parseInput = fromLists . lines

example :: IO String
example = readFile "examples/ex23.txt"

path :: [(Int, Int)]
path = [(1, 2), (2, 2), (2, 3), (2, 4), (2, 5), (2, 6), (2, 7), (2, 8), (3, 8), (4, 4), (4, 5), (4, 6), (4, 7), (4, 8), (5, 4), (6, 4), (7, 4), (8, 4), (8, 5), (8, 6), (9, 6), (10, 2), (10, 3), (10, 4), (10, 5), (10, 6), (11, 2), (12, 2), (12, 4), (12, 5), (12, 6), (13, 2), (13, 4), (13, 6), (14, 2), (14, 3), (14, 4), (14, 6), (15, 6), (16, 2), (16, 3), (16, 4), (16, 5), (16, 6), (17, 2), (18, 2), (18, 3), (18, 4), (18, 8), (18, 9), (18, 10), (19, 4), (19, 8), (19, 10), (20, 2), (20, 3), (20, 4), (20, 6), (20, 7), (20, 8), (20, 10), (20, 12), (20, 13), (20, 14), (20, 15), (20, 16), (20, 18), (20, 19), (20, 20), (21, 2), (21, 6), (21, 10), (21, 12), (21, 16), (21, 18), (21, 20), (22, 2), (22, 3), (22, 4), (22, 5), (22, 6), (22, 10), (22, 11), (22, 12), (22, 16), (22, 17), (22, 18), (22, 20), (22, 21), (22, 22)]

updateGrid :: Grid -> Grid
updateGrid grid = foldr (setElem 'O') grid path
