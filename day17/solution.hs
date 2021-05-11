#!/usr/bin/env stack
{- stack 
  script
  --resolver lts-17.5   
  --package "containers"
  --optimize
  --ghc-options=-no-keep-hi-files 
  --ghc-options=-no-keep-o-files 
-}

module Main (main) where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )

import Data.List ( intercalate )

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do

  handle <- openFile "input.txt" ReadMode
  contents <-  hGetContents handle

  print "Part1"
  let space = parseSpace contents
  (print. Map.size. Map.filter id. exc) (simulateSpace space 6)
  
  
  print "Part2"
  let space4 = parseSpace4 contents
  (print. Map.size. Map.filter id. exc4) (simulateSpace4 space4 6)

  return ()

{-
  "Part1"
  372
  "Part2"
  1896
-}

--quick note: 
--  I could use Space as a newtype of Map position cube
--  but i needed the show instance to debug, although it can be 
--  removed, i think its better to leave it there, in case anyone
--  ever wants to print the cubes again

data Position = Pos Int Int Int
  deriving (Eq, Ord, Show)

type Cube = Bool

newtype Space = SP {exc :: Map Position Cube}

instance Show Space where
  show (SP sp) = intercalate "\n\n" ["z = " ++ show z ++ "\n" ++ face z
                                            | z <- [iz..az]]
    where 
      face z = intercalate "\n" (map (line z) [iy..ay])
      line z y = concatMap  (cell z y) [ix..ax]
      cell z y x = if sp Map.! Pos x y z then "#" else "."

      (Pos ix iy iz, _) = Map.findMin sp
      (Pos ax ay az, _) = Map.findMax sp

simulateSpace :: Space -> Int -> Space
simulateSpace sp 0   = sp
simulateSpace (SP sp) lim = simulateSpace (SP newsp) (lim-1)
  where
    newsp          = Map.mapWithKey updateCube (exc $expand (SP sp))
    updateCube p state
      | state && (2 == activeNbrs || 3 == activeNbrs) = True 
      | not state && ( 3 == activeNbrs)               = True
      | otherwise                                     = False
        where 
          activeNbrs = Map.size $ Map.filter id (exc $ neighbors (SP sp) p)    


expand :: Space -> Space
expand (SP sp) = SP $ foldl (\m k -> Map.insert k False m) sp  faces
  where
    (Pos ix iy iz, _) = Map.findMin sp
    (Pos ax ay az, _) = Map.findMax sp
    face1 = [Pos (ix-1) y' z' | y' <- [(iy-1)..(ay+1)], z' <- [(iz-1)..(az+1)]]
    face2 = [Pos (ax+1) y' z' | y' <- [(iy-1)..(ay+1)], z' <- [(iz-1)..(az+1)]]
    face3 = [Pos x' (iy-1) z' | x' <- [ix..ax],     z' <- [(iz-1)..(az+1)]]
    face4 = [Pos x' (ax+1) z' | x' <- [ix..ax],     z' <- [(iz-1)..(az+1)]]
    face5 = [Pos x' y' (iz-1) | x' <- [ix..ax],     y' <- [iy..ay]]
    face6 = [Pos x' y' (az+1) | x' <- [ix..ax],     y' <- [iy..ay]]
    faces = concat  [face1,face2,face3,face4,face5,face6]



neighbors :: Space -> Position -> Space
neighbors (SP sp) p@(Pos x y z) = SP $ sp `Map.intersection` candidates 
  where
    candidates = Map.fromList [ (Pos (x+x') (y+y') (z+z') , False)
                  | x' <- [-1,0,1]
                  , y' <- [-1,0,1]
                  , z' <- [-1,0,1]
                  , x' /= y' || y'/=z' || z' /= 0] --not a neighbor of itself



data Position4 = Pos4 Int Int Int Int
  deriving (Eq, Ord, Show)


newtype Space4 = SP4 {exc4 :: Map Position4 Cube}

instance Show Space4 where
  show (SP4 sp) = intercalate "\n\n" ["z = " ++ show z ++ " w = " ++ show w ++ "\n" ++ face w z
                                            | z <- [iz..az]
                                            , w <- [iw..aw]]
    where 
      face w z = intercalate "\n" (map (line w z) [iy..ay])
      line w z y = concatMap  (cell w z y) [ix..ax]
      cell w z y x = if sp Map.! Pos4 x y z w then "#" else "."

      (Pos4 ix iy iz iw, _) = Map.findMin sp
      (Pos4 ax ay az aw, _) = Map.findMax sp

simulateSpace4 :: Space4 -> Int -> Space4
simulateSpace4 sp 0   = sp
simulateSpace4 (SP4 sp) lim = simulateSpace4 (SP4 newsp) (lim-1)
  where
    newsp          = Map.mapWithKey updateCube (exc4 $ expand4 (SP4 sp))
    updateCube p state
      | state && (2 == activeNbrs || 3 == activeNbrs) = True 
      | not state && ( 3 == activeNbrs)               = True
      | otherwise                                     = False
        where 
          activeNbrs = Map.size $ Map.filter id (exc4 $ neighbors4 (SP4 sp) p)    


expand4 :: Space4 -> Space4
expand4 (SP4 sp) = SP4 $ foldl (\m k -> Map.insert k False m) sp  faces
  where
    (Pos4 ix iy iz iw, _) = Map.findMin sp
    (Pos4 ax ay az aw, _) = Map.findMax sp
    face1 = [Pos4 (ix-1) y' z' w' | y' <- [(iy-1)..(ay+1)], z' <- [(iz-1)..(az+1)], w'<- [(iw-1)..(aw+1)]]
    face2 = [Pos4 (ax+1) y' z' w' | y' <- [(iy-1)..(ay+1)], z' <- [(iz-1)..(az+1)], w'<- [(iw-1)..(aw+1)]]
    face3 = [Pos4 x' (iy-1) z' w' | x' <- [ix..ax],         z' <- [(iz-1)..(az+1)], w'<- [(iw-1)..(aw+1)]]
    face4 = [Pos4 x' (ax+1) z' w' | x' <- [ix..ax],         z' <- [(iz-1)..(az+1)], w'<- [(iw-1)..(aw+1)]]
    face5 = [Pos4 x' y' (iz-1) w' | x' <- [ix..ax],         y' <- [iy..ay],         w'<- [(iw-1)..(aw+1)]]
    face6 = [Pos4 x' y' (az+1) w' | x' <- [ix..ax],         y' <- [iy..ay],         w'<- [(iw-1)..(aw+1)]]
    face7 = [Pos4 x' y' z' (iw-1) | x' <- [ix..ax],         y' <- [iy..ay],         z'<- [iz..az]]
    face8 = [Pos4 x' y' z' (aw+1) | x' <- [ix..ax],         y' <- [iy..ay],         z'<- [iz..az]]
    faces = concat  [face1,face2,face3,face4,face5,face6,face7,face8]



neighbors4 :: Space4 -> Position4 -> Space4
neighbors4 (SP4 sp) p@(Pos4 x y z w) = SP4 $ sp `Map.intersection` candidates 
  where
    candidates = Map.fromList [ (Pos4 (x+x') (y+y') (z+z') (w+w'), False)
                  | x' <- [-1,0,1]
                  , y' <- [-1,0,1]
                  , z' <- [-1,0,1]
                  , w' <- [-1,0,1]
                  , x'/=y' || y'/=z' || z'/=w' || w'/=0] --not a neighbor of itself




-- Parsing methods
parseSpace :: String -> Space
parseSpace = SP . Map.fromList . concat . zipWith f1 [0..] . lines
  where 
    f1 iy xs  = zipWith (f2 iy) xs [0..]
    f2 iy x ix = (Pos ix iy 0,  x == '#')

parseSpace4 :: String -> Space4
parseSpace4 = SP4 . Map.fromList . concat . zipWith f1 [0..] . lines
  where 
    f1 iy xs  = zipWith (f2 iy) xs [0..]
    f2 iy x ix = (Pos4 ix iy 0 0,  x == '#')
