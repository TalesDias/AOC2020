#!/usr/bin/env stack
{- stack
   script
   --resolver lts-17.5
-}

module Main where

import Control.Monad (void)
import System.IO (IOMode (ReadMode), char8, hClose, hGetContents, openFile, utf16)
import Data.List ( sort )


type Passport = [String]

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle

  let boardingPasses =  lines contents
  let ids = map calculateSeatID boardingPasses

  print "Part1"
  print . maximum $ ids
    

  print "Part2"
  print . findMissingSeat $ sort ids

  hClose handle


findMissingSeat :: [Int] -> Int
findMissingSeat (x:y:xs)
  | x == pred y = findMissingSeat (y:xs)
  | otherwise = succ x

calculateSeatID :: String -> Int 
calculateSeatID seat = 
  let 
    row    = sum $ zipWith (\n z -> if z == 'F' then 0 else 2^n) [6,5..] (take 7 seat)
    column = sum $ zipWith (\n z -> if z == 'L' then 0 else 2^n) [2,1,0] (drop 7 seat)
  in  row * 8 + column