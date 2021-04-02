#!/usr/bin/env stack
{- stack
   script
   --resolver lts-17.5
-}

module Main(main) where

import Data.Functor (($>))
import Control.Applicative ((<*))
import Data.List ( nub ) 
import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) ) 
import Text.Parsec.Char
    ( char, digit, endOfLine, letter, space, string )
import Text.ParserCombinators.Parsec
    ( choice,
      endBy,
      eof,
      many1,
      manyTill,
      optional,
      (<?>),
      parse,
      GenParser )


type Color = String 

data Bag = Bag {bagColor :: Color, containedBags :: [(Int,Color)]}
instance Eq Bag where
  (==) (Bag corA _) (Bag corB _) = corA == corB
instance Show Bag where
  show (Bag cor xs) = cor ++"->"++ show (length xs)

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle

  let bags = parse parseRules "" contents

  print "Part1"
  case bags of 
    Right bs -> (print . length . nub) $ allParentBags "shinygold" bs
    Left e -> print e
  
  print "Part2"
  case bags of 
    Right bs -> (print . length) $ allChildrenBags "shinygold" bs
    Left e -> print e

  hClose handle

{-
  "Part1"
  248
  "Part2"
  57281
-}

allParentBags :: Color -> [Bag] -> [Bag]
allParentBags c bs = diretos ++ concatMap (flip allParentBags bs . bagColor) diretos
  where 
    diretos = filter (elem c . map snd . containedBags) bs


-- funcao mais complicada que já fiz
-- demora um pouquinho pra rodar, mas como ela percorre uma arvore,
-- aint é desculpável
allChildrenBags :: Color -> [Bag] -> [Bag]
allChildrenBags c bs = children ++ concatMap ((`allChildrenBags` bs) . bagColor) children
  where 
    children = 
      map (`firstBagByColor` bs) 
      . concatMap (uncurry replicate) 
      . containedBags 
      $ firstBagByColor c bs

firstBagByColor :: Color -> [Bag] -> Bag
firstBagByColor c = head . filter ((c==) . bagColor)

-- Parsing methods
number :: GenParser Char st Int
number = fmap read (many1 digit) <?> "number"

word :: GenParser Char st String
word = many1 letter <?> "word"
  
color :: GenParser Char st Color
color = fmap (++) word <* space <*> word

noBags :: GenParser Char st [(Int,Color)]
noBags =  [] <$ string "no other bags" 

specificBagRule :: GenParser Char st (Int, Color)
specificBagRule = fmap (,) number <* space <*> color <* space <* word 

parseRule :: GenParser Char st Bag
parseRule = 
  fmap Bag color 
  <* string " bags contain "
  <*> choice [noBags, endBy specificBagRule (optional (string ", "))]
  <* char '.'
  <* endOfLine

parseRules :: GenParser Char st [Bag]
parseRules = manyTill parseRule eof