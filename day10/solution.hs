#!/usr/bin/env stack
{- stack
   script
   --resolver lts-17.5
-}

{-# LANGUAGE Safe #-}
{-# LANGUAGE TupleSections #-}

module Main(main) where

import safe System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )

import safe Data.List ( foldl', sort, partition )

import safe qualified Data.Sequence as S 


main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle

  let adapters = sort .  map (\x -> read x :: Int) $ lines contents
  
  
  print "Part1"
  print . (\(a,b) -> length a * length b) . partition (==1) . calculateGaps $ adapters
  
  
  print "Part2"
  print . flip distinctArrangmets 0 . S.fromList . ((0,1):) . map (,0) $  adapters

  hClose handle

{-
  "Part1"
  1755
  "Part2"
  4049565169664
-}

calculateGaps :: [Int] -> [Int]
calculateGaps = (3:) . tail . foldl' (\(y:ys) x -> x:(x-y):ys) [0]

distinctArrangmets :: S.Seq(Int, Int) -> Int -> Int
distinctArrangmets xs pos
  | S.length xs == pos = snd $ S.index xs (pos-1)
  | otherwise = distinctArrangmets xs123 (pos+1)
  where 
    (atual, incr) = S.index xs pos
    xs1   = tryToVisit xs   incr (1 + atual)
    xs12  = tryToVisit xs1  incr (2 + atual)
    xs123 = tryToVisit xs12 incr (3 + atual)

tryToVisit :: S.Seq(Int, Int) -> Int -> Int -> S.Seq(Int, Int)
tryToVisit xs incr ad = 
  case S.findIndexL (\x -> fst x == ad) xs  of
    Just index -> S.adjust' (fmap (+incr)) index xs 
    Nothing -> xs