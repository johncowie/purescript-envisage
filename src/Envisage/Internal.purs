module Envisage.Internal
( EnvError(..)
, ReadResult(..)
, Defaulted
, VarInfo
, Var(..)
, class Compiler
, class ReadValue
, readValue
, compileParser

, defaultTo
, withParser
, withShow
, describe
)
where

import Prelude

import Data.Either (Either, either)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, lookup)
import Prim.Row (class Cons, class Lacks) as Row
import Prim.RowList (class RowToList, kind RowList, Cons, Nil)
import Record as Record
import Type.Data.RowList (RLProxy(..))
import Type.Equality (class TypeEquals, to)
import Type.RowList (class ListToRow)

type Env = Object String

data Var t = Var { varName :: String
                 , description :: Maybe String
                 , parser :: String -> Either String t
                 , default :: Maybe t
                 , showValue :: Maybe (t -> String)
                 }

type VarInfo = { varName :: String
               , description :: Maybe String
               , default :: Maybe String
               }

type Defaulted = Boolean

data ReadResult = MissingError VarInfo
                | ParseError VarInfo String
                | ValueSupplied VarInfo (Maybe String)
                | DefaultUsed VarInfo
                | OptionalNotSupplied VarInfo

data EnvError = EnvError (Array ReadResult)

describe :: forall t. String -> Var t -> Var t
describe desc (Var r) = Var $ r {description = Just desc}

defaultTo :: forall t. t -> Var t -> Var t
defaultTo def (Var r) = Var $ r {default = Just def}

withParser :: forall t. (String -> Either String t) -> Var t -> Var t
withParser parser (Var r) = Var $ r {parser = parser}

withShow :: forall t. (t -> String) -> Var t -> Var t
withShow showVal (Var r) = Var $ r {showValue = Just showVal}

varInfo :: forall t. Var t -> VarInfo
varInfo (Var {varName, default, description, showValue})
  = {varName, description, default: showValue <*> default}

class ReadValue t where
  readValue :: Var t -> Maybe String -> (Tuple ReadResult (Maybe t))

parseError :: forall t. Var t -> String -> Tuple ReadResult (Maybe t)
parseError var err = Tuple (ParseError (varInfo var) err) Nothing

missingError :: forall t. Var t -> Tuple ReadResult (Maybe t)
missingError var = Tuple (MissingError (varInfo var)) Nothing

success :: forall t. Var t -> t -> Tuple ReadResult (Maybe t)
success var@(Var {showValue}) val = Tuple (ValueSupplied (varInfo var) valStrM) (Just val)
  where valStrM = showValue <*> Just val

defaultUsed :: forall t. Var t -> t -> Tuple ReadResult (Maybe t)
defaultUsed var val = Tuple (DefaultUsed (varInfo var)) (Just val)

optionalMissing :: forall t. Var t -> Tuple ReadResult (Maybe t)
optionalMissing var = Tuple (OptionalNotSupplied (varInfo var)) Nothing

instance readValueMaybe :: ReadValue (Maybe t) where
  readValue var@(Var {parser}) (Just str) = either (parseError var) (success var) $ parser str
  readValue var@(Var {default})  Nothing = maybe (optionalMissing var) (defaultUsed var) default
else instance readValueAll :: ReadValue t where
  readValue var@(Var {parser}) (Just str) = either (parseError var) (success var) $ parser str
  readValue var@(Var {default}) Nothing = maybe (missingError var) (defaultUsed var) default

-- FIXME Use Writer monad for this?
readValueFromEnv :: forall t. (ReadValue t) => Var t -> Object String -> Tuple ReadResult (Maybe t)
readValueFromEnv v@(Var {varName, default}) env = readValue v $ lookup varName env

-- TODO rename this typeclass
class Compiler (el :: RowList) (rl :: RowList) (e :: # Type) (r :: # Type) | el -> rl where
  compileParser :: forall proxy. proxy el -> proxy rl -> (Record e) -> Object String -> Tuple (Array ReadResult) (Maybe (Record r))

instance compilerResultsNil :: (TypeEquals {} (Record r)) => Compiler Nil Nil p r where
  compileParser _ _ _ _ = Tuple [] $ pure $ to {}
else instance compilerCons ::
  ( IsSymbol l
  , ReadValue t
  , Row.Lacks l rt
  , Row.Lacks l pt
  , ListToRow rlt rt
  , ListToRow plt pt
  , Row.Cons l (Var t) pt p
  , Row.Cons l t rt r
  , Compiler plt rlt pt rt
  ) => Compiler (Cons l (Var t) plt) (Cons l t rlt) p r where
    compileParser _ _ vars env = Tuple allResults $ Record.insert name <$> value <*> tail
      where name = (SProxy :: SProxy l)
            (var :: Var t) = Record.get name vars
            (Tuple readResult value) = readValueFromEnv var env
            varsTail = Record.delete name vars
            (Tuple readResults tail) = compileParser (RLProxy :: RLProxy plt) (RLProxy :: RLProxy rlt) varsTail env
            allResults = [readResult] <> readResults
else instance compilerConsSubVars ::
  ( IsSymbol l
  , Row.Lacks l rt
  , Row.Lacks l pt
  , ListToRow rlt rt
  , ListToRow plt pt
  , RowToList v vlt
  , RowToList t tlt
  , ListToRow vlt v
  , ListToRow tlt t
  , Row.Cons l (Record v) pt p
  , Row.Cons l (Record t) rt r
  , Compiler plt rlt pt rt
  , Compiler vlt tlt v t
  ) => Compiler (Cons l (Record v) plt) (Cons l (Record t) rlt) p r where
    compileParser _ _ vars env = Tuple allResults $ Record.insert name <$> value <*> tail
      where name = (SProxy :: SProxy l)
            (subVars :: Record v) = Record.get name vars
            (Tuple subReadResults value) = compileParser (RLProxy :: RLProxy vlt) (RLProxy :: RLProxy tlt) subVars env
            varsTail = Record.delete name vars
            (Tuple readResults tail) = compileParser (RLProxy :: RLProxy plt) (RLProxy :: RLProxy rlt) varsTail env
            allResults = readResults <> subReadResults

-- TODO support showing if value is default

-- Refactoring steps
-- [X] Remove ParsedResult type, replace with EnvError
-- [X] Rename EnvError to ReadResult
-- [X] Change readValue to return Tuple ReadResult (Either Unit r)
-- [X] Replace (Either unit r) with Maybe r
-- [X] Remove SingleError from EnvErrors
-- [X] Rename EnvErrors to EnvError
-- [X] Use 'note' to add convert resulting tuple to either
-- [X] Add Boolean to ReadResult indicating if default value was used
-- [X] Create DefaultUsed ReadResult
-- [X] Create MissingOptional ReadResult
-- [ ] Sort readresults by error type (ParseError, Missing, Mandatory - value supplied, Optional - default supplied, Optional - value not supplied)
-- [ ] Prettify report
