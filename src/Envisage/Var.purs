module Envisage.Var where

import Prelude
import Envisage (Var(..))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Int as Int
import Data.String as Str

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

class MaybeShow t where
  maybeShow :: Maybe (t -> String)

instance maybeShowShow :: Show t => MaybeShow t where
  maybeShow = Just show
else instance maybeShowOther :: MaybeShow a where
  maybeShow = Nothing

var :: forall t. (MaybeShow t) => (ParseValue t) => String -> Var t
var varName = Var { varName
                  , description: Nothing
                  , default: Nothing
                  , showValue: maybeShow
                  , parser: parseValue }
