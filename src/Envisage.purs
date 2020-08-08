module Envisage
( class ReadEnv
, readEnv
, module Envisage.Internal
)
where

import Data.Either (Either, note)
import Data.Tuple (Tuple(..))
import Envisage.Internal as Internal
import Envisage.Internal (Var(..), VarInfo, ReadResult(..), EnvError(..), defaultTo, withParser, withShow, describe)
import Foreign.Object (Object)
import Prim.RowList (class RowToList)
import Type.RowList (class ListToRow)
import Type.Data.RowList (RLProxy(..))

class ReadEnv (e :: # Type) (r :: # Type) where
  readEnv :: (Record e) -> Object String -> Either Internal.EnvError (Record r)

instance readEnvImpl ::
  ( RowToList e el
  , RowToList r rl
  , Internal.Compiler el rl e r
  , ListToRow rl r
  , ListToRow el l
  ) => ReadEnv e r where
    readEnv vars env = note (EnvError readResults) res
      where (Tuple readResults res) = Internal.compileParser (RLProxy :: RLProxy el) (RLProxy :: RLProxy rl) vars env
