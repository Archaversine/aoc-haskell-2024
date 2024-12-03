module Day2 (runDay) where

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

runFile :: String -> FilePath -> ([[Int]] -> Int) ->  IO () 
runFile header path f = do 
    contents <- parseInput <$> readFile path
    putStrLn $ "> " ++ header
    putStrLn $ "Value: " ++ show (f contents)

runDay :: IO ()
runDay = do 
    putStrLn "--== DAY TWO ==--"

    runFile "Test"   "day2-test.txt"  (countSafes isSafe)
    runFile "Part 1" "day2-part1.txt" (countSafes isSafe)

    runFile "Test 2" "day2-test.txt"  (countSafes tolerantSafe)
    runFile "Part 2" "day2-part1.txt" (countSafes tolerantSafe)

