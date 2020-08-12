module Envisage.Example where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Console as Console
import Envisage (Var, defaultTo, describe, readEnv, showParsed, withShow, Component(..), initComponents, mkComponent)
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

component1 :: Component String
component1 = mkComponent { a: (var "A" :: Var Boolean)
                         , b: (var "B" :: Var String)} show

component2 :: Component String
component2 = mkComponent { c: (var "C" :: Var Int)
                         , d: var "D" :: Var Int} show

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
