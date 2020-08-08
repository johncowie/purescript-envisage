module Envisage.Console
( printErrorsForConsole )
where

import Prelude
import Data.Maybe (fromMaybe)
import Data.String as Str
import Chalk as Chalk
import Envisage (VarInfo, EnvError(..), EnvErrors(..))

printMissingError :: VarInfo -> String
printMissingError {varName, description} =
  Chalk.red $ "Missing: " <> Chalk.yellow varName <> " - " <> fromMaybe "<no-description>" description

printErrorForConsole :: EnvError -> String
printErrorForConsole (MissingError info) = printMissingError info
printErrorForConsole (ParseError {varName, description} err) =
  Chalk.red $ "ParseError: " <> Chalk.yellow varName <> " - " <> err
printErrorForConsole (ValueSupplied {varName} valStrM) =
  Chalk.green $ "ValueSupplied: " <> Chalk.yellow varName <> " [" <> fromMaybe "<value-not-shown>" valStrM <> "]"

printErrorsForConsole :: EnvErrors -> String
printErrorsForConsole (MultipleErrors errors) = Str.joinWith "\n" $ map printErrorForConsole errors
printErrorsForConsole (SingleError err) = printErrorForConsole err
