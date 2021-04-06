#!/usr/bin/env stack
{- stack
   script
   --resolver lts-17.5
-}

module Main(main) where

import Control.Applicative ((<*))
import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Text.Parsec.Char ( endOfLine, letter )
import Text.ParserCombinators.Parsec
    ( eof, many1, manyTill, (<?>), parse, GenParser )
import Data.List (foldl', nub, intersect)

type Answer = String
type Group = [Answer]

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle

  let bags = parse answers "" contents

  print "Part1"
  case bags of 
    Right an -> (print. sum. map distinctAnswers) an
    Left e -> print e
  
  print "Part2"
  case bags of 
    Right an -> (print. sum. map equalAnswers) an
    Left e -> print e
  
  hClose handle

{-
  "Part1"
  6612
  "Part2"
  3268
-}

distinctAnswers :: Group -> Int 
distinctAnswers = length . nub . concat

equalAnswers :: Group -> Int
equalAnswers = length . foldl1 intersect

-- Parsing methods
word :: GenParser Char st String
word = many1 letter <?> "word"

answer :: GenParser Char st Answer
answer = word <* endOfLine 

group :: GenParser Char st Group
group = manyTill answer endOfLine

answers :: GenParser Char st [Group]
answers = manyTill group eof