{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.Set
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

getNumber :: Int -> [Char]
getNumber x
    | x == 1 = "one"
    | x == 2 = "two"
    | x == 3 = "three"
    | x == 4 = "four"
    | x == 5 = "five"
    | x == 6 = "six"
    | x == 7 = "seven"
    | x == 8 = "eight"
    | x == 9 = "nine"
    | x == 10 = "ten"
    | x == 11 = "eleven"
    | x == 12 = "twelve"
    | x == 13 = "thirteen"
    | x == 14 = "fourteen"
    | x == 15 = "quarter"
    | x == 16 = "sixteen"
    | x == 17 = "seventeen"
    | x == 18 = "eighteen"
    | x == 19 = "nineteen"
    | x == 20 = "twenty"
    | x == 21 = "twenty one"
    | x == 22 = "twenty two"
    | x == 23 = "twenty three"
    | x == 24 = "twenty four"
    | x == 25 = "twenty five"
    | x == 26 = "twenty six"
    | x == 27 = "twenty seven"
    | x == 28 = "twenty eight"
    | x == 29 = "twenty nine"
    | x == 30 = "half"
    
getString :: Int -> Int -> [Char]
getString h m
    | m == 0 = (getNumber h) ++ " o' clock"
    | m == 15 =  (getNumber m) ++ " past " ++ (getNumber h)
    | m == 30 =  (getNumber m) ++ " past " ++ (getNumber h)
    | m <= 30 = (getNumber m) ++ " minutes past " ++ (getNumber h)
    | m == 45 = (getNumber (60 - m)) ++ " to " ++ (getNumber (h + 1))
    | otherwise = (getNumber (60 - m)) ++ " minutes to " ++ (getNumber (h + 1))

-- Complete the timeInWords function below.
timeInWords h m = do
    getString h m

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    h <- readLn :: IO Int

    m <- readLn :: IO Int

    let result = timeInWords h m

    hPutStrLn fptr result

    hFlush fptr
    hClose fptr
