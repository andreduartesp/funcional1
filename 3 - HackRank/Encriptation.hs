-- https://www.hackerrank.com/challenges/encryption/problem
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
 
getMinString :: [Char] -> Int -> [Char]
getMinString s i
    | i < Data.List.length s = [(!!) s i]
    | otherwise = ""


getString :: [[Char]] -> Int -> Int -> [Char]
getString xs i t
    | i >= t = []
    | otherwise = Data.List.reverse (Data.List.foldl (\x y -> getMinString y i ++ x) "" xs) ++ " " ++ getString xs ((+) i 1) t


breakString :: [Char] -> Int -> [[Char]]
breakString str size
    | length str > size = Data.List.take size str : breakString (Data.List.drop size str) size
    | otherwise = [str]
    
getColumns :: [Char] -> Int
getColumns = ceiling . sqrt  . fromIntegral . length

encryption s = do
    getString (breakString s (getColumns s)) 0 (getColumns s)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    s <- getLine

    let result = encryption s

    hPutStrLn fptr result

    hFlush fptr
    hClose fptr
