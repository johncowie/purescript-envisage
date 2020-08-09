module Envisage.Console
( printErrorsForConsole )
where

import Prelude

import Chalk (gray, green, red, white, yellow)
import Data.Array (sortWith)
import Data.List (List(Nil), fromFoldable, toUnfoldable, (:))
import Data.List as List
import Data.Foldable (maximum)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.String as Str
import Data.Tuple (Tuple(..))
import Envisage (EnvError(..), ReadResult(..), VarInfo)

padTo :: Int -> String -> String
padTo n s
  | n > Str.length s = padTo n (s <> " ")
  | otherwise = s

resultInfo :: ReadResult -> VarInfo
resultInfo (MissingError info) = info
resultInfo (ParseError info _) = info
resultInfo (ValueSupplied info _) = info
resultInfo (DefaultUsed info) = info
resultInfo (OptionalNotSupplied info) = info

varNameWidth :: Array VarInfo -> Int
varNameWidth = map _.varName >>> map Str.length >>> maximum >>> fromMaybe 0

type StringF = String -> String
data Token = VarName StringF | Description | Status StringF String | Value (Maybe String) | DefaultValue | NoValue | None

printToken :: VarInfo -> Token -> String
printToken varInfo (VarName style) = style $ varInfo.varName
printToken varInfo (Description) = maybe (gray "<no-description>") white varInfo.description
printToken varInfo (Status style msg) = style msg
printToken varInfo (Value valStr) = green (fromMaybe "<not-shown>" valStr)
printToken varInfo DefaultValue = yellow $ (fromMaybe "<not-shown>" varInfo.default <> " (default)")
printToken varInfo NoValue = gray "-"
printToken _ None = ""

runTokens :: VarInfo -> Array Token -> List String
runTokens varInfo tokens = fromFoldable (map (printToken varInfo) tokens)

printErrorForConsole :: ReadResult -> List String
printErrorForConsole (MissingError info) =
  runTokens info [VarName red, Status red "[REQUIRED]", NoValue, Description]
printErrorForConsole (ParseError info err) =
  runTokens info [VarName red, Status red "[INVALID]", Status red err, Description]
printErrorForConsole (ValueSupplied info valStr) =
  runTokens info [VarName green, Status green "[SUPPLIED]", Value valStr, Description]
printErrorForConsole (DefaultUsed info) =
  runTokens info [VarName yellow, Status yellow "[OPTIONAL]", DefaultValue, Description]
printErrorForConsole (OptionalNotSupplied info) =
  runTokens info [VarName yellow, Status yellow "[OPTIONAL]", NoValue, Description]

alignRow :: List Int -> List String -> List String
alignRow (p:ps) (x:xs) = padTo p x : alignRow ps xs
alignRow _ _ = Nil

colWidths :: List (List String) -> List Int
colWidths Nil = Nil
colWidths lists = fromMaybe 0 (maximum heads) : colWidths tails
  where heads = map Str.length $ List.catMaybes $ map List.head lists
        tails = List.catMaybes $ map List.tail lists

alignColumns :: List (List String) -> List (List String)
alignColumns arr = map (alignRow widths) arr
  where widths = colWidths arr

tableToString :: List (List String) -> String
tableToString = map (toUnfoldable >>> Str.joinWith " ") >>> toUnfoldable >>> Str.joinWith "\n"

resultOrder :: ReadResult -> Tuple Int String
resultOrder (MissingError {varName}) = Tuple 1 varName
resultOrder (ParseError {varName} _) = Tuple 0 varName
resultOrder (ValueSupplied {varName} _) = Tuple 2 varName
resultOrder (DefaultUsed {varName}) = Tuple 3 varName
resultOrder (OptionalNotSupplied {varName}) = Tuple 4 varName

printErrorsForConsole :: EnvError -> String
printErrorsForConsole (EnvError results) =
  tableToString $
  alignColumns $
  map printErrorForConsole $
  fromFoldable $
  sortWith resultOrder $
  results

-- TODO space out info, get all var names lined up in a column
-- TODO data structure for representing tokens in line
