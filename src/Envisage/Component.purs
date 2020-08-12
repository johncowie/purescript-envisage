module Envisage.Component
( EnvisageComponent
, Component(..)
, initComponents )
where

import Prelude

import Data.Either (Either, note)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Envisage.Internal (EnvError(..), EnvisageInternal, readEnv')
import Envisage.Internal as Internal
import Envisage.Logger (ReaderLoggerT, runReaderLoggerT)
import Envisage.Record (class RecordUpdate, class HasFunction, recordUpdate)
import Foreign.Object (Object)
import Prim.RowList (class RowToList)
import Type.Data.RowList (RLProxy(..))

data EnvisageComponent = EnvisageComponent
data Component e r c = Component e (r -> c)

instance hasFunctionReadComponent :: (
  RowToList e el
, RowToList r rl
, RecordUpdate (ReaderLoggerT (Object String) (Array Internal.ReadResult) Maybe) el rl EnvisageInternal e r
) => HasFunction EnvisageComponent (Component (Record e) (Record r) c) (ReaderLoggerT (Object String) (Array Internal.ReadResult) Maybe c) where
  getFunction :: EnvisageComponent
              -> (Component (Record e) (Record r) c)
              -> ReaderLoggerT (Object String) (Array Internal.ReadResult) Maybe c
  getFunction EnvisageComponent = readComponent

readComponent :: forall e el r rl c.
                 RowToList e el
              => RowToList r rl
              => RecordUpdate (ReaderLoggerT (Object String) (Array Internal.ReadResult) Maybe) el rl EnvisageInternal e r
              => (Component (Record e) (Record r) c)
              -> ReaderLoggerT (Object String) (Array Internal.ReadResult) Maybe c
              -- FIXME replace recordUpdate with version of readEnv that hides EnvisageInternal
readComponent (Component vars cstr) = cstr <$> readEnv' vars

initComponents :: forall c o cl ol.
                  RowToList o ol
               => RowToList c cl
               => RecordUpdate (ReaderLoggerT (Object String) (Array Internal.ReadResult) Maybe) cl ol EnvisageComponent c o
               => Object String
               -> Record c
               -> Either EnvError (Record o)
initComponents env components = note (EnvError readResults) res
  where (Tuple readResults res) = runReaderLoggerT env $ recordUpdate (RLProxy :: RLProxy cl) (RLProxy :: RLProxy ol) EnvisageComponent components
