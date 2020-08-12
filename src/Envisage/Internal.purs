module Envisage.Internal
( ReadResult(..)
, VarInfo
, Var(..)
, class ReadValue
, readValue

, defaultTo
, withParser
, withShow
, describe
, showParsed
, readValueFromEnv
)
where

import Prelude

import Data.Either (Either, either)
import Data.Maybe (Maybe(..), maybe)
import Envisage.Logger (LoggerT, loggerT, ReaderLoggerT, readerLoggerT)
import Foreign.Object (Object, lookup)

type Env = Object String

data Var t = Var { varName :: String
                 , description :: Maybe String
                 , parser :: String -> Either String t
                 , default :: Maybe t
                 , showValue :: t -> Maybe String
                 }

type VarInfo = { varName :: String
               , description :: Maybe String
               , default :: Maybe String
               }

data ReadResult = MissingError VarInfo
                | ParseError VarInfo String
                | ValueSupplied VarInfo (Maybe String)
                | DefaultUsed VarInfo
                | OptionalNotSupplied VarInfo

describe :: forall t. String -> Var t -> Var t
describe desc (Var r) = Var $ r {description = Just desc}

defaultTo :: forall t. t -> Var t -> Var t
defaultTo def (Var r) = Var $ r {default = Just def}

withParser :: forall t. (String -> Either String t) -> Var t -> Var t
withParser parser (Var r) = Var $ r {parser = parser}

showParsed :: forall t. (Show t) => Var t -> Var t
showParsed = withShow show

withShow :: forall t. (t -> String) -> Var t -> Var t
withShow showVal (Var r) = Var $ r {showValue = map Just showVal}

varInfo :: forall t. Var t -> VarInfo
varInfo (Var {varName, default, description, showValue})
  = {varName, description, default: join $ showValue <$> default}

class ReadValue t where
  readValue :: Var t -> Maybe String -> LoggerT (Array ReadResult) Maybe t

parseError :: forall t. Var t -> String -> LoggerT (Array ReadResult) Maybe t
parseError var err = loggerT [ParseError (varInfo var) err] Nothing

missingError :: forall t. Var t -> LoggerT (Array ReadResult) Maybe t
missingError var = loggerT [MissingError (varInfo var)] Nothing

success :: forall t. Var t -> t -> LoggerT (Array ReadResult) Maybe t
success var@(Var {showValue}) val = loggerT [ValueSupplied (varInfo var) valStrM] (Just val)
  where valStrM = showValue val

defaultUsed :: forall t. Var t -> t -> LoggerT (Array ReadResult) Maybe t
defaultUsed var val = loggerT [DefaultUsed (varInfo var)] (Just val)

optionalMissing :: forall t. Var t -> LoggerT (Array ReadResult) Maybe t
optionalMissing var = loggerT [OptionalNotSupplied (varInfo var)] Nothing

instance readValueMaybe :: ReadValue (Maybe t) where
  readValue var@(Var {parser}) (Just str) = either (parseError var) (success var) $ parser str
  readValue var@(Var {default})  Nothing = maybe (optionalMissing var) (defaultUsed var) default
else instance readValueAll :: ReadValue t where
  readValue var@(Var {parser}) (Just str) = either (parseError var) (success var) $ parser str
  readValue var@(Var {default}) Nothing = maybe (missingError var) (defaultUsed var) default

readValueFromEnv :: forall t. (ReadValue t) => Var t -> ReaderLoggerT Env (Array ReadResult) Maybe t
readValueFromEnv v@(Var {varName, default}) = readerLoggerT $ \env -> readValue v $ lookup varName env
