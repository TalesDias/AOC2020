#!/usr/bin/env stack
{- stack
   script
   --resolver lts-17.5
-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Safe #-}


module Main(main) where


import safe System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import safe Text.Parsec.Char
    ( char, digit, endOfLine, letter, space )
import safe Text.ParserCombinators.Parsec
    ( choice, eof, many1, manyTill, (<?>), parse, GenParser )
import safe qualified Data.Sequence as S 
import safe Data.Either ( fromRight,  )


data Instruction 
  = CInstruction { operation :: String
                  , argument :: Int
                  } 
 deriving Show


main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle

  let instructions = S.fromList <$> parse parseInstructions "" contents
  
  print "Part1"
  case instructions of 
    Right ins ->  print .  evalInstructions 0 0 . fmap (,0)  $ ins
    Left e -> print e
  
  
  print "Part2"
  case instructions of 
    Right ins -> print . flip findCorruptedLine 0 $ ins
    Left e -> print e
  
  hClose handle

{-
  "Part1"
  2051
  "Part2"
  2304
-}

-- Não tenho orgulho dessas funções,
-- apenas não sei hs o suficiente p/
-- refatorar, no futuro devo muda-las

-- Evaluate till a infinite loop or end of instructions
evalInstructions :: Int -> Int -> S.Seq (Instruction, Int)  -> Either Int Int
evalInstructions acc posAtual state
  | posAtual >= S.length state = Right acc
  | visited  >= 1             = Left acc
  | operation ins == "acc"    = evalInstructions (acc + argument ins) (posAtual + 1) newState
  | operation ins == "jmp"    = evalInstructions acc (posAtual + argument ins) newState
  | otherwise                 = evalInstructions acc (posAtual + 1) newState
  where 
    current = S.index state posAtual
    (ins, visited) = current
    newState = S.adjust' (fmap (+1)) posAtual state 

findCorruptedLine :: S.Seq Instruction -> Int -> Int
findCorruptedLine ins posAtual
  | "acc" == operation curr = fcl ins (posAtual+1)
  | "nop" == operation curr = fcl ins (posAtual+1) `fromRight` evalInstructions 0 0 (fmap (,0) (S.adjust' (swapOp "jmp") posAtual ins))
  | "jmp" == operation curr = fcl ins (posAtual+1) `fromRight` evalInstructions 0 0 (fmap (,0) (S.adjust' (swapOp "nop") posAtual ins))
  where 
    fcl = findCorruptedLine
    curr = S.index ins posAtual

-- Eu poderia implementar Functor p/ Intruction, 
-- mas deixo para meu futuro eu que virá refatorar
swapOp :: String -> Instruction -> Instruction
swapOp str (CInstruction _ ar) = CInstruction str ar

-- Parsing methods
number :: GenParser Char st Int
number = fmap read (many1 digit) <?> "number"

signedNumber :: GenParser Char st Int
signedNumber = do 
  sign <- choice [char '+', char '-']
  num <- number
  let signed = if sign == '+' then num else -num
  return signed

word :: GenParser Char st String
word = many1 letter <?> "word"

parseInstruction :: GenParser Char st Instruction
parseInstruction = 
  CInstruction <$> word <* space <*> signedNumber <* endOfLine 

parseInstructions :: GenParser Char st [Instruction]
parseInstructions = manyTill parseInstruction eof