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
import Text.ParserCombinators.Parsec

import Debug.Trace

main :: IO ()
main = do

  contents <- parseFromFile contentParser "input.txt"
  
  case contents of
    Left e -> print e
    Right expression -> do

      print "Part1"
      (print. sum. map eval) expression

      print "Part2"
      (print. sum. map evalAdvanced) expression

  return ()

{-
  "Part1"
  464478013511
  "Part2"
  85660197232452
-}

data Expr = Val Int
          | Sum Expr Expr
          | Mul Expr Expr
          | Par Expr
  deriving Show



eval :: Expr -> Int
eval (Val n) = n
eval ex = res
  where 
    Val res = (evmul. evsum. evpar) ex

    evpar (Val n)       = Val n
    evpar (Par n)       = Val (eval n)
    evpar (Sum ex1 ex2) = Sum (evpar ex1) (evpar ex2)
    evpar (Mul ex1 ex2) = Mul (evpar ex1) (evpar ex2)

    evsum (Val n)                          = Val n
    evsum (Sum (Val n1) (Val n2))          = Val (n1 + n2)
    evsum (Sum (Val n1) (Sum (Val n2) ex)) = evsum $ Sum (Val (n1 + n2)) ex
    evsum (Sum (Val n1) (Mul (Val n2) ex)) = evmul $ Mul (Val (n1 + n2)) ex
    evsum m@(Mul ex1 ex2)                  = evmul m

    evmul (Val n)                          = Val n
    evmul (Mul (Val n1) (Val n2))          = Val (n1 * n2)
    evmul (Mul (Val n1) (Mul (Val n2) ex)) = evmul $ Mul (Val (n1 * n2)) ex
    evmul (Mul (Val n1) (Sum (Val n2) ex)) = evsum $ Sum (Val (n1 * n2)) ex
    evmul s@(Sum ex1 ex2)                  = evsum s


evalAdvanced :: Expr -> Int
evalAdvanced (Val n) = n
evalAdvanced ex = res
  where 
    Val res = (evmul. evsum. evpar) ex

    evpar (Val n)       = Val n
    evpar (Par n)       = Val (evalAdvanced n)
    evpar (Sum ex1 ex2) = Sum (evpar ex1) (evpar ex2)
    evpar (Mul ex1 ex2) = Mul (evpar ex1) (evpar ex2)

    evsum (Val n)                          = Val n
    evsum (Sum (Val n1) (Val n2))          = Val (n1 + n2)
    evsum (Sum (Val n1) (Sum (Val n2) ex)) = evsum (Sum (Val (n1 + n2)) ex)
    evsum (Sum (Val n1) (Mul (Val n2) ex)) = Mul (Val (n1 + n2)) (evsum ex)
    evsum (Mul ex1 ex2)                    = Mul (evsum ex1) (evsum ex2)
    evsum a = error (show a)

    evmul (Val n)                          = Val n
    evmul (Mul (Val n1) (Val n2))          = Val (n1 * n2)
    evmul (Mul (Val n1) (Mul (Val n2) ex)) = evmul (Mul (Val (n1*n2)) ex)
    evmul a = error (show a)


-- Parsing methods
number :: GenParser Char st Int
number = read <$> many1 digit <?> "number"

value :: GenParser Char st Expr
value = Val <$> number <|> parenthesis

summation :: GenParser Char st Expr
summation = Sum <$> value <* string " + " <*> expression

multiplication :: GenParser Char st Expr
multiplication = Mul <$> value <* string " * " <*> expression

parenthesis :: GenParser Char st Expr
parenthesis = Par <$> (string "(" *> expression <* char ')')

expression :: GenParser Char st Expr
expression = choice [ try summation
                    , try multiplication
                    , parenthesis
                    , value]


contentParser :: GenParser Char st [Expr]
contentParser = manyTill (expression <* endOfLine) eof