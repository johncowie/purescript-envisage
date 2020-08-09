module Envisage.Var
( class ParseValue
, parseValue
, var
, var')
where

import Prelude
import Envisage.Internal (Var(..))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Int as Int
import Data.String as Str
import Data.Number as Number

class ParseValue v where
  parseValue :: String -> Either String v

instance parseValueMaybe :: (ParseValue v) => ParseValue (Maybe v) where
  parseValue s = Just <$> parseValue s

instance parseValueInt :: ParseValue Int where
  parseValue s = case Int.fromString s of
    (Just i) -> Right i
    Nothing -> Left "Invalid int"

instance parseValueString :: ParseValue String where
  parseValue = Right

instance parseValueBoolean :: ParseValue Boolean where
  parseValue s =
    case Str.toLower s of
      "0" -> Right false
      "false" -> Right false
      "1" -> Right true
      "true" -> Right true
      _ -> Left "Invalid boolean value"

instance parseValueNumber :: ParseValue Number where
  parseValue s = case Number.fromString s of
    (Just i) -> Right i
    Nothing -> Left "Invalid number"

var :: forall t. (ParseValue t) => String -> Var t
var varName = Var { varName
                  , description: Nothing
                  , default: Nothing
                  , showValue: const Nothing
                  , parser: parseValue }

var' :: forall t. String -> (String -> Either String t) -> Var t
var' varName parser = Var { varName
                          , description: Nothing
                          , default: Nothing
                          , showValue: const Nothing
                          , parser: parser }
