#!/usr/bin/env stack
{- stack
   script
   --resolver lts-17.5
-}

{-# LANGUAGE Safe #-}


module Main(main) where

import safe System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import safe Data.List ( union )


main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle

  let numbers = map (\x -> read x :: Int) $ lines contents
  
  
  print "Part1"
  print . findWrongNumber $ numbers
  
  
  print "Part2"
  let contSeq = contiguousSequence numbers
  print $ maximum contSeq + minimum contSeq

  hClose handle

{-
  "Part1"
  2089807806
  "Part2"
  245848639
-}

contiguousSequence :: [Int] -> [Int]
contiguousSequence xs = contSeq' 1 xs
  where  
    target = findWrongNumber xs
    contSeq'  sz xs 
      | currenSum > target = contSeq'  (sz - 1) (tail xs)
      | currenSum < target = contSeq'  (sz + 1) xs
      | otherwise = take sz xs
      where currenSum = sum (take sz xs)


findWrongNumber :: [Int] -> Int
findWrongNumber [] = error "sequence cannot be empty"
findWrongNumber xs
  | candValid = findWrongNumber $ tail xs
  | otherwise = candidate
  where 
    candidate = xs !! 25
    preamble  = take 25 xs
    candValid = candidate `elem` genPairsSum preamble


genPairsSum :: [Int] -> [Int]
genPairsSum [ ] = []
genPairsSum [x] = [] -- a pair needs two different elements
genPairsSum (x:xs) = pairs `union` genPairsSum xs
  where pairs = map (+x) xs