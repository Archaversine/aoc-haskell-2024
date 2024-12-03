module Day2 (runDay2) where

import Day

parseInput :: String -> [[Int]]
parseInput = map (map read . words) . lines

delta :: [Int] -> [Int]
delta (x:xs@(y:_)) = x - y : delta xs
delta _ = []

allEqual :: Eq a => [a] -> Bool 
allEqual [] = True
allEqual (x:xs) = all (== x) xs

isSafe :: [Int] -> Bool 
isSafe xs = allEqual signs && not (all (== 0) signs) && all (<= 3) (map abs d)
    where d     = delta xs
          signs = map signum d

deleteNth :: Int -> [a] -> [a] 
deleteNth _ [] = [] 
deleteNth 0 (_:xs) = xs
deleteNth n (x:xs) = x : deleteNth (n - 1) xs

tolerantSafe :: [Int] -> Bool
tolerantSafe xs 
    | isSafe xs = True
    | otherwise = or [isSafe (deleteNth i xs) | i <- [0 .. length xs - 1]]

countSafes :: ([Int] -> Bool) -> [[Int]] -> Int 
countSafes f = length . filter id . map f

runDay2 :: IO ()
runDay2 = runDay 2 (WithFunction parseInput) [
        ("Test 1", "day2-test.txt" , countSafes isSafe),
        ("Part 1", "day2-part1.txt", countSafes isSafe),

        ("Test 2", "day2-test.txt" , countSafes tolerantSafe),
        ("Part 2", "day2-part1.txt", countSafes tolerantSafe)
    ]
