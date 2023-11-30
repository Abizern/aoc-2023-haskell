module Main where

import Day01 (solve)
import Text.Printf (printf)

getRawInput :: Int -> IO String
getRawInput d = readFile (printf "inputs/day%02d.txt" d)

runSolution :: Int -> (String -> String) -> IO ()
runSolution d solver = do
  input <- getRawInput d
  putStrLn $ printf "Day %02d" d ++ "\n" ++ solver input

main :: IO ()
main = do
  putStrLn "Advent of Code 2023"
  putStrLn "-------------------"
  runSolution 1 Day01.solve
