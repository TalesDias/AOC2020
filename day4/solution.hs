#!/usr/bin/env stack
{- stack
   script
   --resolver lts-17.5
-}

module Main where

import Control.Monad (void)
import System.IO (IOMode (ReadMode), char8, hClose, hGetContents, openFile, utf16)
import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import Data.Char

type Passport = [String]

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle

  let passports = parse parsePassports "Error" contents

  print "Part1"
  case passports of
    Right x -> print . length . filter id . map validatePassportPart1 $ x
    Left y -> print y  
    

  print "Part2"
  case passports of
    Right x -> print . length . filter id . map validatePassportPart2 $ x
    Left y -> print y

  hClose handle

validatePassportPart1 :: Passport -> Bool
validatePassportPart1 pass
  | length pass == 8 = True
  | length pass == 7 && not (any (startsWith "cid") pass) = True
  | otherwise = False
  
validatePassportPart2 :: Passport -> Bool
validatePassportPart2 pass
  | length pass == 8 = validateFields pass
  | length pass == 7 && not (any (startsWith "cid") pass) = validateFields pass
  | otherwise = False

validateFields :: Passport -> Bool
validateFields = all passRule

passRule (x1:x2:x3:_:value)  = case [x1,x2,x3] of {
    "byr" -> value >= "1920" && value <= "2002" && length value == 4;
    "iyr" -> value >= "2010" && value <= "2020" && length value == 4;
    "eyr" -> value >= "2020" && value <= "2030" && length value == 4;
    "hgt" -> (\y -> case take 2 y of { "mc" -> (reverse . drop 2) y >= "150" && (reverse . drop 2) y <= "193"; "ni" -> (reverse . drop 2) y >= "59" && (reverse . drop 2) y <= "76"; _ -> False ;}) $ reverse value;
    "hcl" -> length value == 7 && all isHexDigit (tail value);
    "ecl" -> elem value $ words "amb blu brn gry grn hzl oth";
    "pid" -> length value == 9;
    "cid" -> True;
      _   -> False;
    }
  

startsWith :: String -> String -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith (x : xs) (y : ys)
  | x == y = startsWith xs ys
  | otherwise = False

-- Parsing methods
number :: GenParser Char st Int
number = do
  n <- many1 digit
  return (read n :: Int) <?> "Expecting number"

field :: GenParser Char st String
field = do
  key <- many1 lower <?> "abc"
  void $ char ':'
  value <- try . many . choice $ [digit, lower, char '#']
  return (key ++ ":" ++ value)

parsePassport :: GenParser Char st Passport
parsePassport = field `endBy` try space

parsePassports :: GenParser Char st [Passport]
parsePassports = parsePassport `endBy` choice [char '&', endOfLine]
