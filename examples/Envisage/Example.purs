module Envisage.Example where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Console as Console
import Envisage (Var, defaultTo, describe, readEnv, showParsed, withShow, Component(..), initComponents)
import Envisage.Console (printErrorsForConsole)
import Envisage.Var (var, var', class ParseValue)
import Node.Process (getEnv)

parseUnit :: String -> Either String Unit
parseUnit _ = Right unit

varWithF :: forall t. (ParseValue t) => String -> (Var t -> Var t) -> Var t
varWithF name f = var name # f

infix 9 varWithF as ##

data X = X

parseX :: String -> Either String X
parseX "x" = Right X
parseX _ = Left "Not x"

exampleEnv :: { a :: Var Int
              , b :: Var String
              , c :: {c1 :: Var Int, c2 :: Var Int}
              , d :: Var (Maybe String)
              , e :: Var Boolean
              , f :: Var Number
              , g :: Var Unit
              , h :: Var X}
exampleEnv = { a: "BILL" ## describe "Bill is an int" # defaultTo 7 # showParsed
             , b: "BEN" ## describe "Ben is a string" # showParsed
             , c: { c1: "C1" ## identity
                  , c2: "C2" ## identity}
             , d: "MAYBE" ## describe "Value is optional"
             , e: "BOOL" ## describe "A boolean"
             , f: "NUMBER" ## describe "A number"
             , g: var' "UNIT" parseUnit # describe "A unit (no typeclass)" # withShow (const "()")
             , h: var' "X" parseX }

type Component1Env = { a :: Var Int
                    , b :: Var String}

type Component1Config = {a :: Int, b :: String}

component1 :: Component Component1Env Component1Config String
component1 = Component { a: var "A"
                       , b: var "B"} show

type Component2Env = {c :: Var Boolean, d :: Var Boolean}
type Component2Config = {c :: Boolean, d :: Boolean}

component2 :: Component Component2Env Component2Config String
component2 = Component { c: var "C", d: var "D"} show

main :: Effect Unit
main = do
  env <- getEnv
  case readEnv exampleEnv env of
    (Left err) -> Console.error (printErrorsForConsole err)
    (Right val) -> Console.log "SUCCESS"
  -- case readComponent component1 env of
  --   (Left err) -> Console.error (printErrorsForConsole err)
  --   (Right val) -> Console.log $ show val
  -- case readComponent component2 env of
  --   (Left err) -> Console.error (printErrorsForConsole err)
  --   (Right val) -> Console.log $ show val
  case initComponents env {component1, component2} of
    (Left err) -> Console.error (printErrorsForConsole err)
    (Right cs) -> Console.log (show cs)
