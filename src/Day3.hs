{-# LANGUAGE LambdaCase #-}

module Day3 (runDay, runDay3) where

import Data.Functor

import Day

data SkipMode = Skip | DontSkip deriving Eq

data ParseResult 
    = Mul (Int, Int) 
    | Do 
    | Dont
    | Other Char 
    | EOF

parseMul :: Parser (Int, Int)
parseMul = do 
    a <- string "mul(" *> some digitChar
    b <- char ',' *> some digitChar <* char ')'

    return (read a, read b)

parseParseResult :: Parser ParseResult 
parseParseResult = choice [ Mul   <$> try parseMul
                          , string "don't()" $> Dont
                          , string "do()"    $> Do
                          , Other <$> latin1Char
                          , pure EOF
                          ]

parseMuls :: SkipMode -> Parser [(Int, Int)]
parseMuls skip = parseParseResult >>= \case
    Mul p -> do 
        rest <- parseMuls skip
        pure $ if skip == DontSkip then p : rest else rest
    Do   -> parseMuls DontSkip
    Dont -> parseMuls Skip
    Other _ -> parseMuls skip
    EOF -> pure []

parseMulsWithoutSkips :: Parser [(Int, Int)]
parseMulsWithoutSkips = parseParseResult >>= \case 
    Mul p -> do 
        rest <- parseMulsWithoutSkips
        pure (p : rest)
    EOF -> pure []
    _   -> parseMulsWithoutSkips

processResult :: [(Int, Int)] -> Int 
processResult = sum . map (uncurry (*))

runDay3 :: IO () 
runDay3 = do
    printDayTitle 3

    runFile "Test"   "day3-test.txt"  (WithParser parseMulsWithoutSkips) processResult
    runFile "Part 1" "day3-part1.txt" (WithParser parseMulsWithoutSkips) processResult

    runFile "Test 2" "day3-test2.txt" (WithParser (parseMuls DontSkip)) processResult
    runFile "Part 2" "day3-part1.txt" (WithParser (parseMuls DontSkip)) processResult


