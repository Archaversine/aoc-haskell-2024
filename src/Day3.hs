{-# LANGUAGE LambdaCase #-}

module Day3 (runDay) where

import Data.Functor
import Data.Void

import Text.Megaparsec 
import Text.Megaparsec.Char

type Parser = Parsec Void String

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

        if (skip == DontSkip) then
            pure (p : rest)
        else pure rest
    Do   -> parseMuls DontSkip
    Dont -> parseMuls Skip
    Other _ -> parseMuls skip
    EOF -> pure []

parseMulsWithoutSkips :: Parser [(Int, Int)]
parseMulsWithoutSkips = parseParseResult >>= \case 
    Mul p -> do 
        rest <- parseMulsWithoutSkips
        pure (p : rest)
    Do      -> parseMulsWithoutSkips 
    Dont    -> parseMulsWithoutSkips 
    Other _ -> parseMulsWithoutSkips
    EOF     -> pure []

processResult :: [(Int, Int)] -> Int 
processResult = sum . map (uncurry (*))

runFile :: String -> FilePath -> (SkipMode -> Parser [(Int, Int)]) -> IO () 
runFile header path p = do 
    putStrLn $ "> " ++ header

    contents <- readFile path 
    case parse (p DontSkip) "<day3-input>" contents of 
        Left err -> putStrLn (errorBundlePretty err)
        Right rs -> putStrLn $ "Value: " ++ show (processResult rs)

runDay :: IO () 
runDay = do 
    putStrLn "--== DAY THREE ==--"

    runFile "Test"   "day3-test.txt"  (const parseMulsWithoutSkips)
    runFile "Part 1" "day3-part1.txt" (const parseMulsWithoutSkips)

    runFile "Test 2" "day3-test2.txt" parseMuls
    runFile "Part 2" "day3-part1.txt" parseMuls

