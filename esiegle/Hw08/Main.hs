-- Main.hs
-- Creates an executable program that solves algebraic equations.

module Main where

import Control.Monad ( when )
import System.Exit ( exitSuccess )
import System.IO   ( hFlush, stdout )

import Halgebra ( checkEquation, solve, prettyRational )
import Parser ( parseEquationEither )

-- Print a non-newline-terminated string and flush the output buffer
flushStr str = do
  putStr str
  hFlush stdout

-- Interestingly, loop needs this type signature. It is polymorphic-recursive.
-- Sadly, I can't find a good resource that gives an easy-to-understand definition
-- of polymorphic recursion. Can you?
loop :: IO a
loop = do
  flushStr "Enter an equation to solve, or \"quit\" to quit: "
  line <- getLine

  when (line == "quit")
    exitSuccess

  eqn <- case parseEquationEither line of
           Left err -> do
             putStrLn $ "Parse error: " ++ show err
             loop

           Right eqn -> return eqn

  leqn <- case checkEquation eqn of
            Nothing -> do
              putStrLn "Equation is not linear. I can solve only linear equations."
              loop

            Just leqn -> return leqn

  case solve leqn of
    Nothing -> putStrLn "The variable cancels out: no solution."
    Just r  -> putStrLn $ "Equation solved. x = " ++ prettyRational r

  loop

main :: IO ()
main = do
  putStrLn "Welcome to the algebra solver."
  loop
