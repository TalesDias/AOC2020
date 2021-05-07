#!/usr/bin/env stack
{- stack 
  script
  --resolver lts-17.5   
  --package "parsec MissingH"
  --optimize
  --ghc-options=-no-keep-hi-files 
  --ghc-options=-no-keep-o-files 
-}

module Main(main) where

import Text.Parsec.Char
    ( char, digit, letter, newline, space, string )
import Text.ParserCombinators.Parsec
    ( endBy1,
      eof,
      many1,
      option,
      optional,
      (<?>),
      parseFromFile,
      GenParser )

import Data.Maybe ( isNothing, mapMaybe )
import Data.List ( find, sort, delete, transpose, isPrefixOf ) 


type Field = Int

type Ticket = [Field]

data Range = Range {minR :: Int, maxR :: Int}

type Content = ([Rule], Ticket, [Ticket])

data Rule = Rule {name :: String, r1 :: Range, r2 :: Range}

instance Eq Rule where
  Rule name1 _ _ == Rule name2 _ _ = name1 == name2

instance Ord Rule where
  compare a b = EQ


main :: IO ()
main = do

  contents <- parseFromFile contentParser "input.txt"
  case contents of
    Left e -> print e
    Right (rules, myTicket, nearbyTickets) -> do

      print "Part1"
      print . sum $ mapMaybe (ticketError rules) nearbyTickets

      print "Part2"
      let validTickets = myTicket: filter (isNothing . ticketError rules) nearbyTickets
      let correctLabels =  findFieldLabels validTickets rules

      (print . product . filter (/=0)) $ departureFields correctLabels myTicket

  return ()

{-
  "Part1"
  29759
  "Part2"
  1307550234719
-}

ticketError :: [Rule] -> Ticket -> Maybe Field
ticketError rs [] = Nothing
ticketError rs (t:ts)
  | err = Just t
  | otherwise = ticketError rs ts
  where
    err = not $ any (validateField t) rs

validateField :: Field -> Rule -> Bool
validateField f (Rule _ r1 r2)
  | f >= minR r1 && f <= maxR r1 = True
  | f >= minR r2 && f <= maxR r2 = True
  | otherwise = False

findFieldLabels :: [Ticket] -> [Rule] -> [Rule]
findFieldLabels ts rs = reducePossibilities possibles
  where
    possibles = map (`discardImpossibles` rs) (transpose ts)

discardImpossibles :: [Field] -> [Rule] -> [Rule]
discardImpossibles fs =  filter (\x -> all ( `validateField` x) fs)

reducePossibilities :: [[Rule]] -> [Rule]
reducePossibilities = map snd. sort. reduce. zip [0..]
  where
    reduce :: [(Int,[Rule])] -> [(Int,Rule)]
    reduce [ ] = [ ]
    reduce rss =
        case find (snd. fmap ((==1). length) ) rss of
          Nothing        -> error "reduction not possible"
          Just (ix, r:_) -> (ix, r) : reduce (rest r)
      where
        rest r
          = filter (not. null. snd)
          $ map (fmap $ delete r) rss

departureFields :: [Rule] -> Ticket -> [Int]
departureFields =
  zipWith (\r t -> if "departure" `isPrefixOf` name r then t else 0)

-- Parsing methods
number :: GenParser Char st Int
number = read <$> many1 digit <?> "number"

range :: GenParser Char st Range
range = Range <$> number <* char '-' <*> number

ruleName :: GenParser Char st String
ruleName = (++)
      <$> many1 letter
      <*> option []
      ((:) <$> space <*> many1 letter)

rule :: GenParser Char st Rule
rule = Rule
      <$> ruleName
      <*  char ':' <* space
      <*> range
      <*  string " or "
      <*> range

ticket :: GenParser Char st Ticket
ticket = many1 (number <* optional (char ','))

contentParser :: GenParser Char st Content
contentParser = do
  rules <- endBy1 rule newline
  newline
  string "your ticket:" <* newline
  myTicket <- ticket <* newline
  newline
  string "nearby tickets:" <* newline
  nearbyTickets <-  endBy1 ticket newline
  eof
  return (rules, myTicket, nearbyTickets)