{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ParallelListComp #-}

module Day1 (runDay1) where

import Data.List (sort)

import Day

parseNumber :: Parser Int
parseNumber = read <$> (some digitChar <* hspace)

parsePair :: Parser (Int, Int)
parsePair = do 
    a <- parseNumber 
    b <- parseNumber <* eol

    return (a, b)
    
combineLists :: [(Int, Int)] -> Int 
combineLists zs = sum [abs (x - y) | x <- sort xs | y <- sort ys]
    where (xs, ys) = unzip zs

similarityScore :: [(Int, Int)] -> Int
similarityScore zs = sum [x * length (filter (== x) ys) | x <- xs]
    where (xs, ys) = unzip zs

runDay1 :: IO () 
runDay1 = runDay 1 (WithParser (some parsePair)) [
    ("Test 1", "day1-test.txt" , combineLists),
    ("Part 1", "day1-part1.txt", combineLists),

    ("Test 2", "day1-test.txt" , similarityScore),
    ("Part 2", "day1-part1.txt", similarityScore)
    ]
