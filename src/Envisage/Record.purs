module Envisage.Record where

import Prelude
import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row (class Cons, class Lacks) as Row
import Prim.RowList (Cons, Nil, kind RowList)
import Record as Record
import Type.Data.RowList (RLProxy(..))
import Type.Equality (class TypeEquals, to)
import Type.RowList (class ListToRow)

class HasFunction category x y | x -> y where
  getFunction :: category -> x -> y

class RecordUpdate a (el :: RowList) (rl :: RowList) hf (e :: # Type) (r :: # Type) | el -> rl where
  recordUpdate :: forall proxy. proxy el -> proxy rl -> hf -> (Record e) -> a (Record r)

instance recordUpdateNil :: (Applicative a, TypeEquals {} (Record r)) => RecordUpdate a Nil Nil hf p r where
  recordUpdate _ _ _ _ = pure $ to {}
else instance recordUpdateCons ::
  ( IsSymbol l
  , Row.Lacks l rt
  , Row.Lacks l pt
  , ListToRow rlt rt
  , ListToRow plt pt
  , Row.Cons l x pt p
  , Row.Cons l y rt r
  , Applicative a
  , Apply a
  , HasFunction c x (a y)
  , RecordUpdate a plt rlt c pt rt
  ) => RecordUpdate a (Cons l x plt) (Cons l y rlt) c p r where
    recordUpdate _ _ hf inputs = Record.insert name <$> output <*> outputTail
      where name = (SProxy :: SProxy l)
            (input :: x) = Record.get name inputs
            (output :: a y) = getFunction hf input
            inputTail = Record.delete name inputs
            outputTail = recordUpdate (RLProxy :: RLProxy plt) (RLProxy :: RLProxy rlt) hf inputTail
