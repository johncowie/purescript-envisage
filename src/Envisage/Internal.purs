module Envisage.Internal
( EnvErrors(..)
, EnvError(..)
, ParsedValue
, VarInfo
, Var(..)
, class Compiler
, class ReadValue
, readValue
, compileParser
, addParsedToErrors

, defaultTo
, withParser
, withShow
, describe
)
where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
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

data EnvError = MissingError VarInfo
              | ParseError VarInfo String
              | ValueSupplied VarInfo (Maybe String) -- FIXME bit of a hack

data ParsedValue = ParsedValue VarInfo (Maybe String)

parsedValue :: forall t. Var t -> t -> ParsedValue
parsedValue (Var r) val = ParsedValue (varInfo (Var r)) (r.showValue <*> Just val)

data EnvErrors = SingleError EnvError | MultipleErrors (Array EnvError)

parsedValueToEnvError :: ParsedValue -> EnvError
parsedValueToEnvError (ParsedValue info valStrM) = ValueSupplied info valStrM

addParsedToErrors :: Array ParsedValue -> EnvErrors -> EnvErrors
addParsedToErrors pvs errs = errs <> MultipleErrors (map parsedValueToEnvError pvs)

instance semigroupEnvError :: Semigroup EnvErrors where
  append (MultipleErrors aErrors) (MultipleErrors bErrors) = MultipleErrors (aErrors <> bErrors)
  append (MultipleErrors errors) (SingleError err) = MultipleErrors $ errors <> [err]
  append (SingleError err) (MultipleErrors errors) = MultipleErrors $ [err] <> errors
  append (SingleError errA) (SingleError errB) = MultipleErrors [errA, errB]

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
  readValue :: Var t -> Maybe String -> Either EnvErrors t

instance readValueMaybe :: ReadValue (Maybe t) where
  readValue (Var v) (Just str) = lmap (SingleError <<< ParseError (varInfo (Var v))) $ v.parser str
  readValue (Var v)  Nothing =
    case v.default of
      (Just def) -> Right def
      Nothing -> Right Nothing
else instance readValueAll :: ReadValue t where
  readValue (Var v) (Just str) = lmap (SingleError <<<  ParseError (varInfo (Var v))) $ v.parser str
  readValue (Var v) Nothing =
    case v.default of
      (Just def) -> Right def
      Nothing -> Left $ SingleError $ MissingError (varInfo (Var v))

readValueFromEnv :: forall t. (ReadValue t) => Var t -> Object String -> Either EnvErrors t
readValueFromEnv v@(Var {varName, default}) env = readValue v $ lookup varName env

-- TODO rename this typeclass
class Compiler (el :: RowList) (rl :: RowList) (e :: # Type) (r :: # Type) | el -> rl where
  compileParser :: forall proxy. proxy el -> proxy rl -> (Record e) -> Object String -> Tuple (Array ParsedValue) (Either EnvErrors (Record r)) -- TODO use state monad

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
    compileParser _ _ vars env = insert value tail
      where name = (SProxy :: SProxy l)
            (var :: Var t) = Record.get name vars
            value = readValueFromEnv var env
            varsTail = Record.delete name vars
            (Tuple parsedVals tail) = compileParser (RLProxy :: RLProxy plt) (RLProxy :: RLProxy rlt) varsTail env
            insert (Right val) (Left tailErrs) = Tuple (parsedVals <> [parsedValue var val]) (Left tailErrs)
            insert (Left valueErr) (Left tailErrs) = Tuple parsedVals $ Left $ valueErr <> tailErrs
            insert (Right val) (Right tailS) = Tuple (parsedVals <> [parsedValue var val]) (Right (Record.insert name val tailS))
            insert (Left valueErr) (Right tailS) = Tuple parsedVals $ Left valueErr
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
    compileParser _ _ vars env = insert value tail
      where name = (SProxy :: SProxy l)
            (subVars :: Record v) = Record.get name vars
            (Tuple subParsedVals value) = compileParser (RLProxy :: RLProxy vlt) (RLProxy :: RLProxy tlt) subVars env
            varsTail = Record.delete name vars
            (Tuple parsedVals tail) = compileParser (RLProxy :: RLProxy plt) (RLProxy :: RLProxy rlt) varsTail env
            allParsedVals = subParsedVals <> parsedVals
            insert (Right val) (Left tailErrs) = Tuple allParsedVals (Left tailErrs)
            insert (Left valueErr) (Left tailErrs) = Tuple allParsedVals $ Left $ valueErr <> tailErrs
            insert (Right val) (Right tailS) = Tuple allParsedVals (Right (Record.insert name val tailS))
            insert (Left valueErr) (Right tailS) = Tuple allParsedVals $ Left valueErr
