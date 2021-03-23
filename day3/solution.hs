#!/usr/bin/env stack
{- stack
   script
   --resolver lts-17.5
-}

module Main where 

import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )

type TreeMap = [[Bool]]
type Slope = (Int, Int)

lineLength::Int
lineLength = 31

main :: IO()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle

  let treeMap =  map (map (=='#')) $ lines contents
  print "Primeira parte: "
  print (treesInTrajectory (3,1) treeMap)

  print "Segunda parte: "
  let slopes = [(1,1),(3,1),(5,1),(7,1),(1,2)]
  print . product $ map ( `treesInTrajectory` treeMap) slopes


  hClose handle

treesInTrajectory :: Slope -> TreeMap-> Int 
treesInTrajectory sl = countTrees 0 0 . drop (snd sl)
  where countTrees acc _ [] = acc
        countTrees acc pos tm =
          let 
            newPos = mod (pos + fst sl) lineLength
          in if head tm !! newPos then
                countTrees (acc+1) newPos $ drop (snd sl) tm
            else
                countTrees acc newPos $ drop (snd sl) tm

