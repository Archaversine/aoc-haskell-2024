module Day1 (runDay) where

import Data.List (sort)

import Data.Void

import System.Exit

import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

parseNumber :: Parser Int
parseNumber = read <$> (some digitChar <* hspace)

parsePair :: Parser (Int, Int)
parsePair = do 
    a <- parseNumber 
    b <- parseNumber <* eol

    return (a, b)

parseInput :: String -> Either (ParseErrorBundle String Void) [(Int, Int)]
parseInput xs = parse (some parsePair) "<day1-input>" xs
    
combineLists :: [Int] -> [Int] -> Int 
combineLists xs ys = sum $ zipWith (\x y -> abs (x - y)) (sort xs) (sort ys)

similarityScore :: [Int] -> [Int] -> Int
similarityScore xs ys = sum [x * length (filter (== x) ys) | x <- xs]

runFile :: String -> FilePath -> ([Int] -> [Int] -> Int) -> IO () 
runFile header path f = do 
    input <- readFile path 

    putStrLn $ "> " ++ header
    
    case parseInput input of 
        Left err -> do 
            putStrLn (errorBundlePretty err)
            exitFailure
        Right rs -> do 
            let (xs, ys) = unzip rs
            putStrLn $ "Value: " ++ show (f xs ys)

runDay :: IO ()
runDay = do 
    putStrLn "--== DAY ONE ==--"

    runFile "Test"   "day1-test.txt"  combineLists
    runFile "Part 1" "day1-part1.txt" combineLists

    runFile "Test (Similarity Score)" "day1-test.txt"  similarityScore
    runFile "Part 2"                  "day1-part1.txt" similarityScore

    putStrLn "--== ******* ==--"
