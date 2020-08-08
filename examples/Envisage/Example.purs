module Envisage.Example where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Console as Console
import Envisage (Var, defaultTo, describe, readEnv)
import Envisage.Var (var)
import Envisage.Console (printErrorsForConsole)
import Node.Process (getEnv)

exampleEnv :: { a :: Var Int
              , b :: Var String
              , c :: {c1 :: Var Int, c2 :: Var Int}
              , d :: Var (Maybe String)
              , e :: Var Boolean}
exampleEnv = { a: var "BILL" # describe "Bill is an int" # defaultTo 7
             , b: var "BEN" # describe "Ben is a string"
             , c: {c1: var "C1", c2: var "C2"}
             , d: var "MAYBE" # describe "Value is optional"
             , e: var "BOOL" # describe "A boolean"}

main :: Effect Unit
main = do
  env <- getEnv
  case readEnv exampleEnv env of
    (Left err) -> Console.error (printErrorsForConsole err)
    (Right val) -> Console.log (show val)
