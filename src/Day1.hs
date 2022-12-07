module Day1 where

import Data.List
import Data.List.Split (splitOn)

{--
Problem:

The Elves take turns writing down the number of Calories contained by the various meals, snacks, rations, etc.
that they've brought with them, one item per line.
Each Elf separates their own inventory from the previous Elf's inventory (if any) by a blank line.
--}

filterSplit s = filter (not . null) . splitOn s

-- Count calories
countCalories :: String -> [Int]
countCalories xs = map sum transformed'
  where
    transformed =
      map
        (filter (not . null) . splitOn "\n")
        (filter (not . null) $ splitOn "\n\n" xs)
    transformed' = (map . map) read transformed

-- maximum will give us the elf carrying the most calories
mostCalories :: [Int] -> Int
mostCalories = maximum

topThreeCalories :: [Int] -> [Int]
topThreeCalories = take 3 . sortBy (flip compare)

-- Solve part 1
solvePart1 :: IO Int
solvePart1 = do
  content <- readFile "inputs/day1.txt"
  let answer = mostCalories . countCalories $ content
  pure answer -- return it inside the IO monad

-- Solve part 2
solvePart2 :: IO Int
solvePart2 = do
  content <- readFile "inputs/day1.txt"
  let answer = sum . topThreeCalories . countCalories $ content
  pure answer
