#!/usr/bin/env stack
{- stack script
   --resolver lts-17.5
-}

{-# LANGUAGE Safe #-}

module Main(main) where

import safe System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import safe Data.Maybe ( catMaybes )


data Seat = Floor | Empty | Occupied 
 deriving (Eq,Show)

type Layout = [[Seat]]

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle

  let seats = parseLayout . lines $ contents
  
  print "Part1"
  print . countOccupied . stablizeLayout $ seats

  print "Part2"
  print . countOccupied . stablizeLayout2 $ seats

  hClose handle

{-
  "Part1"
  2316
  "Part2"
  2128
-}

stablizeLayout :: Layout -> Layout
stablizeLayout past
  | past == new = new
  | otherwise   = stablizeLayout new
  where new = nextLayout past

nextLayout :: Layout -> Layout
nextLayout xss = zipWith nextLine [0..] xss
 where nextLine i xs = map (nextSeat xss i) [0..(length xs -1)]

nextSeat :: Layout -> Int -> Int -> Seat
nextSeat lay i j 
  | curr == Empty    && nearby == 0 = Occupied
  | curr == Occupied && nearby >= 4 = Empty
  | otherwise = curr
  where 
    nearby = length . filter (==Occupied) $ neighbourSeats lay i j
    curr   = lay!!i!!j

neighbourSeats :: Layout -> Int -> Int -> [Seat]
neighbourSeats lay i j 
  = catMaybes [sfI j' =<< sfI i' lay | (i', j') <- indexes]
  where
    indexes =
      [(i-1,j-1),(i-1,j),(i-1,j+1), -- upper row
      (i,j-1),           (i,j+1),   -- middle row, without self
      (i+1,j-1), (i+1,j),(i+1,j+1)] -- bottom row


-- Simplismente ignore a repetição
stablizeLayout2 :: Layout -> Layout
stablizeLayout2 past
  | past == new = new
  | otherwise   = stablizeLayout2 new
  where new = nextLayout2 past

nextLayout2 :: Layout -> Layout
nextLayout2 xss = zipWith nextLine2 [0..] xss
 where nextLine2 i xs = map (nextSeat2 xss i) [0..(length xs -1)]

nextSeat2 :: Layout -> Int -> Int -> Seat
nextSeat2 lay i j 
  | curr == Empty    && nearby == 0 = Occupied
  | curr == Occupied && nearby >= 5 = Empty
  | otherwise = curr
  where 
    nearby = length . filter (==Occupied) $ neighbourSeats2 lay i j
    curr = lay!!i!!j

neighbourSeats2 :: Layout -> Int -> Int -> [Seat]
neighbourSeats2 lay i j 
  = catMaybes 
      [seatInDirection lay i j di dj | (di, dj) <- directions]
  where
    directions =
      [(pred,pred),(pred,id),(pred,succ),
       (id,pred),            (id,succ),   
       (succ,pred),(succ,id),(succ,succ)] 

seatInDirection :: Layout -> Int -> Int -> (Int -> Int) -> (Int -> Int) -> Maybe Seat
seatInDirection lay i j upI upJ 
  | curr == Just Floor = seatInDirection lay newI newJ upI upJ 
  | otherwise = curr
  where 
    newI = upI i
    newJ = upJ j
    curr = sfI newJ =<< sfI newI lay


--Alias
sfI = safeIndex

safeIndex :: Int -> [a] -> Maybe a
safeIndex _ [] = Nothing
safeIndex i xs
  | i < 0          = Nothing
  | i >= length xs = Nothing
  | otherwise      = Just (xs !! i)

countOccupied :: [[Seat]] -> Int
countOccupied = sum . map (length . filter (==Occupied))

parseLayout :: [[Char]] -> Layout
parseLayout = map $ map  char2Seat
  where 
    char2Seat x = case x of
      '.' -> Floor
      'L' -> Empty