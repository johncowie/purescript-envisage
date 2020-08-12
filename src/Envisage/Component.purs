module Envisage.Component
( EnvisageComponent
, Component(..)
, initComponents
, mkComponent)
where

import Prelude

import Data.Either (Either, note)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Envisage.Internal (EnvError(..), EnvisageInternal, readEnv')
import Envisage.Internal as Internal
import Envisage.Logger (ReaderLoggerT, runReaderLoggerT)
import Envisage.Record (class RecordUpdate, class HasFunction, recordUpdate, getFunction)
import Foreign.Object (Object)
import Prim.RowList (class RowToList)
import Type.Data.RowList (RLProxy(..))

data EnvisageComponent = EnvisageComponent
data Component o = Component (ReaderLoggerT (Object String) (Array Internal.ReadResult) Maybe o)

mkComponent :: forall e el r rl o.
               RowToList e el
            => RowToList r rl
            => RecordUpdate (ReaderLoggerT (Object String) (Array Internal.ReadResult) Maybe) el rl EnvisageInternal e r
            => (Record e)
            -> (Record r -> o)
            -> Component o
mkComponent vars ctr = Component (ctr <$> config)
  where config = readEnv' vars

instance hasFunctionReadComponent :: HasFunction EnvisageComponent (Component c) (ReaderLoggerT (Object String) (Array Internal.ReadResult) Maybe c) where
  getFunction :: EnvisageComponent
              -> (Component c)
              -> ReaderLoggerT (Object String) (Array Internal.ReadResult) Maybe c
  getFunction EnvisageComponent (Component r) = r

unwrapComponent :: forall c.
                (Component c)
              -> ReaderLoggerT (Object String) (Array Internal.ReadResult) Maybe c
              -- FIXME replace recordUpdate with version of readEnv that hides EnvisageInternal
unwrapComponent = getFunction EnvisageComponent

initComponents :: forall c o cl ol.
                  RowToList o ol
               => RowToList c cl
               => RecordUpdate (ReaderLoggerT (Object String) (Array Internal.ReadResult) Maybe) cl ol EnvisageComponent c o
               => Object String
               -> Record c
               -> Either EnvError (Record o)
initComponents env components = note (EnvError readResults) res
  where (Tuple readResults res) = runReaderLoggerT env $ recordUpdate (RLProxy :: RLProxy cl) (RLProxy :: RLProxy ol) EnvisageComponent components
