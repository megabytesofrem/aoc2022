module Day2 where

import Data.List.Split

{--

Appreciative of your help yesterday, one Elf gives you an encrypted strategy guide (your puzzle input) that they say will be sure to help you win.
"The first column is what your opponent is going to play: A for Rock, B for Paper, and C for Scissors. The second column--"
Suddenly, the Elf is called away to help with someone's tent.

The second column, you reason, must be what you should play in response: X for Rock, Y for Paper, and Z for Scissors.
Winning every time would be suspicious, so the responses must have been carefully chosen.

--}

data RPS = R | P | S
  deriving (Show)

scoreShape :: RPS -> Int
scoreShape x = case x of
  R -> 1
  P -> 2
  S -> 3

-- Return the score
score :: [RPS] -> Int
score [you@R, P] = scoreShape you + 0
score [you@S, P] = scoreShape you + 6
score [you@P, P] = scoreShape you + 3
score [you@R, S] = scoreShape you + 6
score [you@P, S] = scoreShape you + 0
score [you@S, S] = scoreShape you + 3
score [you@S, R] = scoreShape you + 0
score [you@P, R] = scoreShape you + 6
score [you@R, R] = scoreShape you + 3

mapLetters :: String -> RPS
mapLetters c
  | c `elem` ["A", "X"] = R
  | c `elem` ["B", "Y"] = P
  | c `elem` ["C", "Z"] = S
  | otherwise = error "invalid"

filterSplit :: Eq a => [a] -> [a] -> [[a]]
filterSplit s = filter (not . null) . splitOn s

totalScore :: [Char] -> Int
totalScore sg = sum . map (score . reverse) . filter (not . null) $ sg'
  where
    sg' = [map mapLetters $ filterSplit " " x | x <- splitOn "\n" sg]

solvePart1 :: IO Int
solvePart1 = do
  content <- readFile "inputs/day2.txt"
  let answer = totalScore content
  pure answer
