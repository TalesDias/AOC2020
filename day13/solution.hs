#!/usr/bin/env stack
{- stack script
   --resolver lts-17.5
-}

-- {-# LANGUAGE Safe #-}

module Main(main) where

import System.IO
import qualified Data.Text as T

import Data.Ord
import Data.List

newtype Bus = Bus Int
  deriving (Eq, Ord)

data Timestamp = Ignore | Time Int
  deriving (Show, Eq)

instance Ord Timestamp where
  Ignore <= Time _ = True
  Time _ <= Ignore = False
  Time a <= Time b = a <= b


main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- lines <$> hGetContents handle

  let minDepartTime = read (head contents) :: Int
  let buses = parseBuses $ last contents
  let (depart, Bus quickerBus) = minimum $ busWaitTime minDepartTime buses

  print "Part1"
  print (quickerBus * (depart - minDepartTime))


  let timestamps = parseTimestamps $ last contents
  let offsetTime = sortOn Down $ calculateOffsets timestamps

  print "Part2"
  print $ minSincDepart offsetTime 100000000000000

  hClose handle

{-
  "Part1"
  3464
  "Part2"
  XXXXX
-}

busWaitTime :: Int -> [Bus] -> [(Int, Bus)]
busWaitTime depart bs 
  = [(await, Bus b) 
    | Bus b <- bs
    , await <- [minMultAfter depart b]]

minMultAfter :: Int -> Int -> Int
minMultAfter base n = head . filter (>=base) $ [x * n | x <-[1..]]



minSincDepart :: [(Timestamp, Int)] -> Int -> Int
minSincDepart ((Time t,a):tas) d 
    = head [n-a | n <- [0,t..] , match tas n]
  where 
    match :: [(Timestamp, Int)] -> Int -> Bool
    match       []         _ = True
    match ((Ignore,_):_)   _ = True
    match ((Time t,a):tas) d = (d+a) `mod` t == 0 && match tas d

calculateOffsets :: [Timestamp] -> [(Timestamp, Int)]
calculateOffsets = flip zip [0 .. ]



parseTimestamps :: String -> [Timestamp]
parseTimestamps = map 
              ((\t -> if t=="x" then Ignore else  Time (read t :: Int))
              . T.unpack )
             . T.splitOn (T.singleton ',')
             . T.pack

parseBuses :: String -> [Bus]
parseBuses = map (\x -> Bus (read x :: Int))
             . filter (/="x")
             . map T.unpack 
             . T.splitOn (T.singleton ',')
             . T.pack