-- usage: cabal run aoc23 23
-- description: Path lengths in a graph
-- Some really gnarly algothms today.
module Day23 where

import AOCUtils.Input (makeTitle)
import Control.Parallel.Strategies (parMap, rseq)
import qualified Data.List.NonEmpty as NE (fromList, head)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Matrix (Matrix, fromLists, ncols, nrows, (!))
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set (empty, fromList, insert, notMember, singleton, union)

title :: String
title = "--- Day 23: A Long Walk ---"

type Grid = Matrix Char

type Coord = (Int, Int)

type JGraph = Map Coord [(Coord, Int)]

solve :: String -> String
solve s = mconcat ["\n", header, "Part 1: ", part1, "\nPart 2: ", part2, "\n"]
  where
    input = parseInput s
    header = makeTitle title
    part1 = show $ solve1 input
    part2 = show $ solve2 input

solve1 :: Grid -> Int
solve1 = maxNrSteps

solve2 :: Grid -> Int
solve2 _ = 2

isinGrid :: Grid -> Coord -> Bool
isinGrid grid (row, col) = row `elem` [1 .. nrows grid] && col `elem` [1 .. ncols grid]

startPoint :: Grid -> Coord
startPoint grid = NE.head $ NE.fromList [(1, c) | c <- [1 .. ncols grid], grid ! (1, c) == '.']

endPoint :: Grid -> Coord
endPoint grid = NE.head $ NE.fromList $ [(nrows grid, c) | c <- [1 .. ncols grid], grid ! (nrows grid, c) == '.']

junctions :: Grid -> [Coord]
junctions grid =
  [ (r, c)
    | r <- [1 .. nrows grid],
      c <- [1 .. ncols grid],
      grid ! (r, c) /= '#',
      ( length $
          filter (/= '#') $
            map (grid !) $
              filter (isinGrid grid) [(r, pred c), (r, succ c), (pred r, c), (succ r, c)]
      )
        > 2
  ]

makeGraph :: Grid -> JGraph
makeGraph grid = makeGraph' initialGraph (Set.singleton start) [(0, start)] points
  where
    start = startPoint grid
    end = endPoint grid
    forks = junctions grid
    points = [start] ++ forks ++ [end]
    initialGraph = (Map.fromList $ map (\p -> (p, [])) points)
    makeGraph' :: JGraph -> Set Coord -> [(Int, Coord)] -> [Coord] -> JGraph
    makeGraph' graph _ [] [_] = graph
    makeGraph' graph _ [] (_ : j : js) = makeGraph' graph (Set.singleton j) [(0, j)] (j : js)
    makeGraph' graph seen ((n, p) : ss) (j : js)
      | n /= 0 && p `elem` points = makeGraph' (Map.insertWith (++) j [(p, n)] graph) seen ss (j : js)
      | otherwise = makeGraph' graph seen' (ss ++ map (\pt -> (succ n, pt)) candidates) (j : js)
      where
        candidates = nextPoints grid seen p
        ptSet = Set.fromList candidates
        seen' = Set.union seen ptSet
    makeGraph' a b c d = error $ mconcat ["err", show a, "\n", show b, "\n", show c, "\n", show d]

nextPoints :: Grid -> Set Coord -> Coord -> [Coord]
nextPoints grid seen (row, col) = newPoints
  where
    newPoints = filter notForest . filter notSeen . filter inGrid $ candidates
    inGrid = isinGrid grid
    notSeen pt = Set.notMember pt seen
    notForest pt = grid ! pt /= '#'
    candidates = case grid ! (row, col) of
      ch
        | ch == '>' -> [(row, succ col)]
        | ch == '<' -> [(row, pred col)]
        | ch == '^' -> [(pred row, col)]
        | ch == 'v' -> [(succ row, col)]
        | otherwise -> [(row, succ col), (row, pred col), (succ row, col), (pred row, col)]

maxNrSteps :: Grid -> Int
maxNrSteps grid = maxNrSteps' Set.empty 0 start
  where
    graph = makeGraph grid
    start = startPoint grid
    end = endPoint grid
    maxNrSteps' :: Set Coord -> Int -> Coord -> Int
    maxNrSteps' seen steps point
      | point == end = steps
      | otherwise = optimal
      where
        seen' = Set.insert point seen
        neighbours =
          [ (n, pos)
            | (pos, n) <- fromJust $ Map.lookup point graph,
              Set.notMember pos seen
          ]
        optimal = maximum (0 : parMap rseq (uncurry (maxNrSteps' seen' . (steps +))) neighbours)

parseInput :: String -> Grid
parseInput = fromLists . lines

example :: IO String
example = readFile "examples/ex23.txt"
