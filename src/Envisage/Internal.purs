module Envisage.Internal
( ReadResult(..)
, VarInfo
, Var(..)
, EnvError(..)
, EnvisageInternal
, Component
, mkComponent
, class ReadValue
, readValue
, readEnv
, readEnv'

, defaultTo
, withParser
, withShow
, describe
, showParsed
, readValueFromEnv
)
where

import Prelude

import Data.Either (Either, either, note)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Envisage.Logger (LoggerT, loggerT, ReaderLoggerT, readerLoggerT, runReaderLoggerT)
import Foreign.Object (Object, lookup)
import Envisage.Record (class RecordUpdate, recordUpdate, class HasFunction)
import Prim.RowList (class RowToList)
import Type.Data.RowList (RLProxy(..))

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

data Component o = Component (ReaderLoggerT (Object String) (Array ReadResult) Maybe o)

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

optionalMissing :: forall t. Var (Maybe t) -> LoggerT (Array ReadResult) Maybe (Maybe t)
optionalMissing var = loggerT [OptionalNotSupplied (varInfo var)] (Just Nothing)

instance readValueMaybe :: ReadValue (Maybe t) where
  readValue var@(Var {parser}) (Just str) = either (parseError var) (success var) $ parser str
  readValue var@(Var {default})  Nothing = maybe (optionalMissing var) (defaultUsed var) default
else instance readValueAll :: ReadValue t where
  readValue var@(Var {parser}) (Just str) = either (parseError var) (success var) $ parser str
  readValue var@(Var {default}) Nothing = maybe (missingError var) (defaultUsed var) default

readValueFromEnv :: forall t. (ReadValue t) => Var t -> ReaderLoggerT Env (Array ReadResult) Maybe t
readValueFromEnv v@(Var {varName, default}) = readerLoggerT $ \env -> readValue v $ lookup varName env

data EnvError = EnvError (Array ReadResult)

data EnvisageInternal = EnvisageInternal

instance hasFunctionReadVar :: (ReadValue t) => HasFunction EnvisageInternal (Var t) (ReaderLoggerT (Object String) (Array ReadResult) Maybe t) where
  getFunction _ = readValueFromEnv

instance hasFunctionReadRecord :: (
  RowToList e el
, RowToList r rl
, RecordUpdate (ReaderLoggerT (Object String) (Array ReadResult) Maybe) el rl EnvisageInternal e r
) => HasFunction EnvisageInternal (Record e) (ReaderLoggerT (Object String) (Array ReadResult) Maybe (Record r)) where
  getFunction _ = recordUpdate (RLProxy :: RLProxy el) (RLProxy :: RLProxy rl) EnvisageInternal

instance hasFunctionReadComponent :: HasFunction EnvisageInternal (Component c) (ReaderLoggerT (Object String) (Array ReadResult) Maybe c) where
  getFunction :: EnvisageInternal
              -> (Component c)
              -> ReaderLoggerT (Object String) (Array ReadResult) Maybe c
  getFunction EnvisageInternal (Component r) = r

mkComponent :: forall e el r rl o.
               RowToList e el
            => RowToList r rl
            => RecordUpdate (ReaderLoggerT (Object String) (Array ReadResult) Maybe) el rl EnvisageInternal e r
            => (Record e)
            -> (Record r -> o)
            -> Component o
mkComponent vars ctr = Component (ctr <$> config)
  where config = readEnv' vars

readEnv' :: forall e r el rl .
            RowToList e el
         => RowToList r rl
         => RecordUpdate (ReaderLoggerT (Object String) (Array ReadResult) Maybe) el rl EnvisageInternal e r
         => Record e
         -> (ReaderLoggerT (Object String) (Array ReadResult) Maybe) (Record r)
readEnv' = recordUpdate (RLProxy :: RLProxy el) (RLProxy :: RLProxy rl) EnvisageInternal

readEnv :: forall e r el rl .
            RowToList e el
         => RowToList r rl
         => RecordUpdate (ReaderLoggerT (Object String) (Array ReadResult) Maybe) el rl EnvisageInternal e r
         => (Object String)
         -> Record e
         -> Either EnvError (Record r)
readEnv env vars = note (EnvError readResults) res
  where (Tuple readResults res) = runReaderLoggerT env $ readEnv' vars
