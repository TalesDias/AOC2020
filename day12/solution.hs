#!/usr/bin/env stack
{- stack script
   --resolver lts-17.5
-}

{-# LANGUAGE Safe #-}

module Main(main) where

import safe System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import safe Prelude hiding (Left, Right)
import safe Data.List ( foldl' )


data Action 
  = North Int
  | South Int
  | East  Int
  | West  Int
  | Left  Int
  | Right Int
  | Forward Int
 deriving (Eq, Show)

-- (Horizontal, Vertical, Angulo)
type State = (Int,Int,Int)
-- (Horizontal, Vertical, posHorizontal, posVertical)
type StateWP = (Int, Int, Int, Int) 

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle

  let actions = map parseActions $ lines contents
  
  print "Part1"
  print . manhattanDist $ actions

  print "Part2"
  print . manhattanDistWP $ actions

  hClose handle

{-
  "Part1"
  1603
  "Part2"
  52866
-}

manhattanDist :: [Action] -> Int
manhattanDist xs = abs a + abs b
  where (a,b,_) = foldl' performAction (0,0,0) xs

performAction :: State -> Action -> State
performAction (h,v,ang) ac = case ac of
  North   i -> (h,v+i,ang)
  South   i -> (h,v-i,ang)
  East    i -> (h+i,v,ang)
  West    i -> (h-i,v,ang)
  Left    i -> (h,v,ang+i)
  Right   i -> (h,v,ang-i)
  Forward i -> moveForward i
  where
    moveForward i = case mod ang 360 of
      0   -> (h+i,v,ang)
      90  -> (h,v+i,ang)
      180 -> (h-i,v,ang)
      270 -> (h,v-i,ang)
      _   -> error $ "wrong angle value: " ++ show ang


manhattanDistWP :: [Action] -> Int
manhattanDistWP xs = abs posH + abs posV
  where (_,_,posH,posV) = foldl' performActionWP (10,1,0,0) xs

performActionWP :: StateWP -> Action -> StateWP
performActionWP (h,v,  posH,posV) ac = case ac of
  North   i -> (h,v+i,  posH,posV)
  South   i -> (h,v-i,  posH,posV)
  East    i -> (h+i,v,  posH,posV)
  West    i -> (h-i,v,  posH,posV)
  Left    i -> rotate i
  Right   i -> rotate (-i)
  Forward i -> (h,v, posH + i*h, posV + i*v)
  where
    rotate newAng = case mod newAng 360 of
      0   -> (h,  v, posH, posV)
      90  -> (-v, h, posH, posV)
      180 -> (-h,-v, posH, posV) 
      270 -> (v, -h, posH, posV)
      _   -> error $ "wrong angle value: " ++ show newAng



parseActions :: String -> Action
parseActions (x:xs) = (case x of 
  'N' -> North 
  'S' -> South 
  'E' -> East  
  'W' -> West  
  'L' -> Left  
  'R' -> Right 
  'F' -> Forward) (read xs)
