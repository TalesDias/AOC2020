#!/usr/bin/env stack
{- stack 
  script
  --resolver lts-17.5   
  --package "unordered-containers text containers"
  --optimize
  --ghc-options=-no-keep-hi-files 
  --ghc-options=-no-keep-o-files 
-}

-- {-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import  System.IO

import Data.List
import Data.Maybe


import Data.Text ( pack, splitOn, unpack )

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM


main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <-  hGetContents handle

  let numbers = map (read . unpack) . splitOn "," . pack $ contents

  print "Part1"
  print $ playUntil2020 (reverse numbers) (length numbers)
  
  print "Part2"
  --print $ playUntil30MSimple (reverse numbers) (length numbers)
  print $ playUntil30MIM numbers (length numbers)
  --print $ playUntil30MHM numbers (length numbers)
  

  hClose handle

{-
  "Part1"
  981
  "Part2"
  164878
-}


playUntil2020:: [Int] -> Int -> Int
playUntil2020 (x:xs) 2020 = x
playUntil2020 (x:xs) n = 
  case elemIndex x xs of 
    Just ix -> playUntil2020 ((ix+1):x:xs) (n+1)
    Nothing -> playUntil2020 (0:x:xs) (n+1)


-- This time, i'm playing a bit with Hask containers
-- Also, sorry for the similar names, this is just an experiment


{-time: 
real	1669m38.817s
user	880m47.638s
sys	2m21.547s

Note: the code did not finished excecuting
i got bored after more than 1 day waiting
-}

playUntil30MSimple:: [Int] -> Int -> Int
playUntil30MSimple (x:xs) 30000000 = x
playUntil30MSimple (x:xs) n = 
  case elemIndex x xs of 
    Just ix -> playUntil30MSimple ((ix+1):x:xs) (n+1)
    Nothing -> playUntil30MSimple (0:x:xs) (n+1)


-- for maps, a key is the number, and a value is the last time it apeared

--time: ~60s
playUntil30MIM :: [Int] -> Int -> Int
playUntil30MIM xs = find30M (IM.fromList $ zip (init xs) [1..]) (last xs)
  where 
    find30M :: IntMap Int -> Int -> Int -> Int
    find30M _ carry 30000000 = carry
    find30M m carry n = case ix of
      Just ix -> find30M (IM.insert carry n m) (n-ix) (n+1)
      Nothing -> find30M (IM.insert carry n m) 0 (n+1)
      where 
        ix = IM.lookup carry m

--time: ~90s
playUntil30MHM :: [Int] -> Int -> Int
playUntil30MHM xs = find30M (HM.fromList $ zip (init xs) [1..]) (last xs)
  where 
    find30M :: HashMap Int Int -> Int -> Int -> Int
    find30M _ carry 30000000 = carry
    find30M m carry n = case ix of
      Just ix -> find30M (HM.insert carry n m) (n-ix) (n+1)
      Nothing -> find30M (HM.insert carry n m) 0 (n+1)
      where 
        ix = HM.lookup carry m
