#!/usr/bin/env stack
{- stack
   script
   --resolver lts-17.5
-}

module Main where 

import Text.ParserCombinators.Parsec ( char, digit, lower, space, many1, parse, GenParser, (<?>))
import Text.Parsec.Char ( char, digit, lower, space )
import System.IO ( hClose, openFile, hGetContents, IOMode(ReadMode), char8, utf16 )
import Control.Monad (void)

data Policy = Policy {
  minAllowed :: Int,
  maxAllowed :: Int,
  code :: Char
}

type Password = String

main :: IO()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle

  let passwords = mapM (parse parsePasswords "Error") $ lines contents


  print "Part1"
  case  passwords of 
    Right x -> print . length . filter id . map validatePasswordPart1 $ x
    Left y  -> print y

  print "Part2"
  case passwords of 
    Right x -> print . length . filter id . map validatePasswordPart2 $ x
    Left y  -> print y


  hClose handle


number :: GenParser Char st Int
number = do 
  n <- many1 digit
  return (read n ::Int) <?> "Expecting number"

parsePasswords :: GenParser Char st (Policy, Password)
parsePasswords = do
  _min <- number
  void $ char '-'
  _max <- number
  void space 
  _letter <- lower 
  void $ char ':'
  void space 
  password <- many1 lower

  return (Policy {minAllowed=_min, maxAllowed=_max, code=_letter}, password) 


validatePasswordPart1 :: (Policy,Password) -> Bool 
validatePasswordPart1 (pol ,pass) = minAllowed pol <= count && maxAllowed pol >= count
  where count = length . filter (code pol ==) $ pass
  
validatePasswordPart2 :: (Policy,Password) -> Bool 
validatePasswordPart2 (pol, pass) = let
  pos1 = code pol == (pass !! (-1 +minAllowed pol))
  pos2 = code pol == (pass !! (-1 +maxAllowed pol))
  in (pos1 || pos2) && (pos1 /= pos2) --xor

{-
"Part1"
569
"\nPart2"
346
-}