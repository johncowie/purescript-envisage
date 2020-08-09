module Envisage.Var
( class ParseValue
, parseValue
, class MaybeShow
, maybeShow
, var
, newVar)
where

import Prelude
import Envisage (Var(..))
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

class MaybeShow t where
  maybeShow :: (t -> Maybe String)

instance maybeShowMaybe :: Show t => MaybeShow (Maybe t) where
  maybeShow = map show
else instance maybeShowShow :: Show t => MaybeShow t where
  maybeShow = map Just show
else instance maybeShowOther :: MaybeShow t where
  maybeShow = const Nothing

var :: forall t. (MaybeShow t) => (ParseValue t) => String -> Var t
var varName = Var { varName
                  , description: Nothing
                  , default: Nothing
                  , showValue: maybeShow
                  , parser: parseValue }

newVar :: forall t. (MaybeShow t) => String -> (String -> Either String t) -> Var t
newVar varName parser = Var { varName
                            , description: Nothing
                            , default: Nothing
                            , showValue: maybeShow
                            , parser: parser }
