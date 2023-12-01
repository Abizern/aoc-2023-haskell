module Main where

import Day01 (solve)
import Day02 (solve)
import Day03 (solve)
import Day04 (solve)
import Day05 (solve)
import Day06 (solve)
import Day07 (solve)
import Day08 (solve)
import Day09 (solve)
import Day10 (solve)
import Day11 (solve)
import Day12 (solve)
import Day13 (solve)
import Day14 (solve)
import Day15 (solve)
import Day16 (solve)
import Day17 (solve)
import Day18 (solve)
import Day19 (solve)
import Day20 (solve)
import Day21 (solve)
import Day22 (solve)
import Day23 (solve)
import Day24 (solve)
import Day25 (solve)
import System.Environment (getArgs)
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [n] | day `elem` [1 .. 25] -> runDay day
      where
        day = read n
    _ -> print "Please enter a day from 1 to 25"

runDay :: Int -> IO ()
runDay n = do
  input <- getRawInput n
  let solution = solutions !! (n - 1)
  putStrLn $ solution input

getRawInput :: Int -> IO String
getRawInput d = readFile (printf "inputs/day%02d.txt" d)

solutions :: [String -> String]
solutions =
  [ Day01.solve,
    Day02.solve,
    Day03.solve,
    Day04.solve,
    Day05.solve,
    Day06.solve,
    Day07.solve,
    Day08.solve,
    Day09.solve,
    Day10.solve,
    Day11.solve,
    Day12.solve,
    Day13.solve,
    Day14.solve,
    Day15.solve,
    Day16.solve,
    Day17.solve,
    Day18.solve,
    Day19.solve,
    Day20.solve,
    Day21.solve,
    Day22.solve,
    Day23.solve,
    Day24.solve,
    Day25.solve
  ]
