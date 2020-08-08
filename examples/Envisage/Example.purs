module Envisage.Example where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Console as Console
import Envisage (Var, defaultTo, describe, readEnv, withShow)
import Envisage.Var (var, newVar, class MaybeShow, class ParseValue)
import Envisage.Console (printErrorsForConsole)
import Node.Process (getEnv)

parseUnit :: String -> Either String Unit
parseUnit _ = Right unit

varWithF :: forall t. (MaybeShow t) => (ParseValue t) => String -> (Var t -> Var t) -> Var t
varWithF name f = var name # f

infix 9 varWithF as ##

exampleEnv :: { a :: Var Int
              , b :: Var String
              , c :: {c1 :: Var Int, c2 :: Var Int}
              , d :: Var (Maybe String)
              , e :: Var Boolean
              , f :: Var Number
              , g :: Var Unit}
exampleEnv = { a: "BILL" ## describe "Bill is an int" # defaultTo 7
             , b: "BEN" ## describe "Ben is a string"
             , c: { c1: "C1" ## identity
                  , c2: "C2" ## identity}
             , d: "MAYBE" ## describe "Value is optional"
             , e: "BOOL" ## describe "A boolean"
             , f: "NUMBER" ## describe "A number"
             , g: newVar "UNIT" parseUnit # describe "A unit (no typeclass)" # withShow (const "()")}

main :: Effect Unit
main = do
  env <- getEnv
  case readEnv exampleEnv env of
    (Left err) -> Console.error (printErrorsForConsole err)
    (Right val) -> Console.log (show val)
