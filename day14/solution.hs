#!/usr/bin/env stack
{- stack script
   --resolver lts-17.5
-}

{-# LANGUAGE Safe #-}

module Main(main) where

import safe System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import safe Text.Parsec.Char ( alphaNum, digit, endOfLine, string )   
import safe Text.ParserCombinators.Parsec
    ( eof, many1, manyTill, (<?>), (<|>), many, parse, try, GenParser )

import safe qualified Data.Map.Strict as Map


type Memory = Map.Map String String
type Mask = String 

data Command 
  = Store String String --position and value
  | Update Mask

  deriving Show


main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <-  hGetContents handle

  let commands = parse parseCommands "" contents

  print "Part1"
  print $ sum . Map.map bytestr2dec . runCommands <$> commands

  print "Part2"
  print $ sum . Map.map bytestr2dec . runCommandsV2 <$> commands

  hClose handle

{-
  "Part1"
  10050490168421
  "Part2"
  2173858456958
-}


runCommands :: [Command] -> Memory
runCommands = run Map.empty "" 
  where 
    run mem _  [] = mem
    run mem mask ((Update newMask):cs) = run mem newMask cs
    run mem mask ((Store p v):cs) = run (Map.insert p (applyMask mask v) mem) mask cs

applyMask :: Mask -> String -> String
applyMask = zipWith (\m s -> if m == 'X' then s else m)


runCommandsV2 :: [Command] -> Memory
runCommandsV2 = run Map.empty ""
  where 
    run mem _  [] = mem
    run mem mask ((Update newMask):cs) = run mem newMask cs
    run mem mask ((Store p v):cs) = run (Map.union (insertV2 mask p v) mem) mask cs

insertV2 :: Mask -> String -> String -> Memory
insertV2 mask p v = foldr (`Map.insert` v) Map.empty (addresses mask p [[]])
  where 
    addresses   [ ]      [ ]  dss = dss
    addresses (m:mask) (p:ps) dss 
      = addresses mask ps (case m of
        '0' -> map (p:)   dss
        '1' -> map ('1':) dss
        _   -> map ('1':) dss ++ map ('0':) dss
      )

dec2bytestr :: Int -> String
dec2bytestr n = build "" n [2^x | x <-[35,34..0]]
  where 
    build str _ [] = str
    build str n (b:bs) 
      | n >= b    = build (str ++ "1") (n-b) bs
      | otherwise = build (str ++ "0") n bs

bytestr2dec :: String -> Int
bytestr2dec str = colapse 0 str [2^x | x <-[35,34..0]]
  where 
    colapse n  _  [] = n
    colapse n (s:str) (b:bs) 
      | s == '1'  = colapse (n+b) str bs
      | otherwise = colapse   n   str bs


-- Parsing methods
number :: GenParser Char st Int
number = read <$> many1 digit <?> "number"

parseMask :: GenParser Char st Command
parseMask = Update <$> (string "mask = " *> many alphaNum)

parseStore :: GenParser Char st Command
parseStore = Store <$>
              (dec2bytestr <$> (string "mem[" *> number))
              <*> 
              (dec2bytestr <$> (string "] = " *> number))

parseCommand :: GenParser Char st Command
parseCommand = (try parseStore <|> parseMask) <* endOfLine

parseCommands :: GenParser Char st [Command]
parseCommands = manyTill parseCommand eof