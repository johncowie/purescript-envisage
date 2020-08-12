module Envisage
(
  -- class ReadEnv
  readEnv
, EnvError(..)
, module Envisage.Internal
, module Envisage.Var
, EnvisageInternal(..)
)
where

import Prelude

import Data.Maybe (Maybe)
import Data.Either (Either, note)
import Data.Tuple (Tuple(..))
import Envisage.Internal (Var(..), VarInfo, ReadResult(..), defaultTo, withParser, withShow, describe, showParsed)
import Envisage.Internal as Internal
import Envisage.Logger (runReaderLoggerT, ReaderLoggerT)
import Envisage.Record (class RecordUpdate, recordUpdate, class HasFunction)
import Envisage.Var (var, var')
import Foreign.Object (Object)
import Prim.RowList (class RowToList)
import Type.Data.RowList (RLProxy(..))
import Type.RowList (class ListToRow)

data EnvError = EnvError (Array ReadResult)

data EnvisageInternal = EnvisageInternal

instance hasFunctionReadVar :: HasFunction EnvisageInternal (Var t) (ReaderLoggerT (Object String) (Array ReadResult) Maybe t) where
  getFunction _ = Internal.readValueFromEnv

instance hasFunctionReadRecord :: (
  RowToList e el
, RowToList r rl
, ListToRow el e
, ListToRow rl r
, RecordUpdate (ReaderLoggerT (Object String) (Array ReadResult) Maybe) el rl EnvisageInternal e r
) => HasFunction EnvisageInternal (Record e) (ReaderLoggerT (Object String) (Array ReadResult) Maybe (Record r)) where
  getFunction _ = recordUpdate (RLProxy :: RLProxy el) (RLProxy :: RLProxy rl) EnvisageInternal

readEnv :: forall e r el rl .
            RowToList e el
         => RowToList r rl
         => ListToRow el e
         => ListToRow rl r
         => RecordUpdate (ReaderLoggerT (Object String) (Array ReadResult) Maybe) el rl EnvisageInternal e r
         => Record e
         -> Object String
         -> Either EnvError (Record r)
readEnv vars env = note (EnvError readResults) res
  where (Tuple readResults res) = runReaderLoggerT env $ recordUpdate (RLProxy :: RLProxy el) (RLProxy :: RLProxy rl) EnvisageInternal vars

{-
  New thoughts, ideally a component should describe what env vars it needs
  so maybe you start with (Tuple envVars (config -> component)) which returns (Tuple (Array ReadResult) (Maybe component))
  question then is can you do that for multiple components, or continue using the Writer monad to compose the output?

  data Component = Component env (config -> component)

  ^ implement record recursive shenanigans for this type

  so it would like e.g.

  initComponents env {
    oauth: oauthComponent (where oauthComponent = Component oauthVars oauthConstructor)
  }

  or just use a do monad, to combine loading component results - combination of reader and writer?s ReaderT

  runInitComponents :: ReaderT (Object String) (WriterT (Array ReadResult) (Maybe ))

  e.g. toError $ runWriter $ runReader $ do
    oauth <- initComponent oauthComponent
    db <- initComponent dbComponent
    pure {oauth, db}
-}
