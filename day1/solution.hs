#!/usr/bin/env stack
{- stack
   script
   --resolver lts-17.5
-}


module Main where

import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )

main:: IO()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print "Part1:"
  print $ sumTwoEntries $ map readInt (lines contents)

  print "Part2:"
  print $ sumThreeEntries $ map readInt (lines contents)


  hClose handle

readInt :: String -> Int
readInt = read

sumTwoEntries :: [Int] -> Int 
sumTwoEntries xs = 
  let entries = [(x+y, x*y) | x <-xs, y <-xs]
      res = head $ filter ((==2020). fst) entries
  in snd res


sumThreeEntries :: [Int] -> Int
sumThreeEntries xs = 
  let entries =  [(x+y+z, x*y*z) | x <- xs, y <- xs, z <- xs] 
      res = head $ filter ((==2020) . fst) entries
  in  snd res
