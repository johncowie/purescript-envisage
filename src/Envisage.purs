module Envisage
( class ReadEnv
, readEnv
, module Envisage.Internal
, EnvError(..)
)
where

import Prelude
import Control.Monad.Writer (runWriter)
import Data.Either (Either, note)
import Data.Tuple (Tuple(..))
import Envisage.Internal as Internal
import Envisage.Internal (Var(..), VarInfo, ReadResult(..), defaultTo, withParser, withShow, describe)
import Foreign.Object (Object)
import Prim.RowList (class RowToList)
import Type.RowList (class ListToRow)
import Type.Data.RowList (RLProxy(..))

data EnvError = EnvError (Array ReadResult)

class ReadEnv (e :: # Type) (r :: # Type) where
  readEnv :: (Record e) -> Object String -> Either EnvError (Record r)

instance readEnvImpl ::
  ( RowToList e el
  , RowToList r rl
  , Internal.Compiler el rl e r
  , ListToRow rl r
  , ListToRow el l
  ) => ReadEnv e r where
    readEnv vars env = note (EnvError readResults) res
      where (Tuple res readResults) = runWriter $ Internal.compileParser (RLProxy :: RLProxy el) (RLProxy :: RLProxy rl) vars env
