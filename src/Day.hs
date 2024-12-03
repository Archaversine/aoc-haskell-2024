{-# LANGUAGE LambdaCase #-}

module Day ( runFile 
           , runDay
           , printDayTitle
           , printFileValue
           , Parser 
           , PuzzleParser(..)
           , module Text.Megaparsec
           , module Text.Megaparsec.Char
           ) where

import Data.Void 

import System.Exit

import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

data PuzzleParser a = WithParser (Parser a) | WithFunction (String -> a)

printDayTitle :: Int -> IO ()
printDayTitle n = putStrLn $ "--== DAY " ++ show n ++ " ==--"

printFileValue :: Show a => String -> a -> IO ()
printFileValue name value = putStrLn $ " * " ++ name ++ ": " ++ show value

runFile :: Show b => String -> FilePath -> PuzzleParser a -> (a -> b) -> IO ()
runFile name path (WithFunction p) f = (f . p <$> readFile path) >>= printFileValue name
runFile name path (WithParser   p) f = parse p path <$> readFile path >>= \case 
    Left err -> do 
        putStrLn (errorBundlePretty err)
        exitFailure
    Right rs -> putStrLn $ " * " ++ name ++ ": " ++ show (f rs)

runDay :: Show b => Int -> PuzzleParser a -> [(String, FilePath, a -> b)] -> IO ()
runDay n p xs = do 
    printDayTitle n
    mapM_ (\(name, path, f) -> runFile name path p f) xs

