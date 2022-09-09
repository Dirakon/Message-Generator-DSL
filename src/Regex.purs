module Regex where

import Prelude

import Data.String.Regex (Regex)
import Data.String.Regex.Flags (global, noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.String.Utils (escapeRegex)


notPreceededBySlashRegex :: String -> Regex
notPreceededBySlashRegex patten = unsafeRegex ("[^\\\\]" <> escapeRegex  patten <> "") noFlags

assertionOperationRegex:: Regex
assertionOperationRegex = unsafeRegex "(!=)|(==)" noFlags

doubleQuoteBodyGlobal :: Regex
doubleQuoteBodyGlobal = unsafeRegex "\"([^\"]*)\"" global

-- stateRegex :: Regex
-- stateRegex = unsafeRegex "^[A-Z | a-z][A-Z | a-z]$" noFlags
-- nonEmptyRegex :: Regex
-- nonEmptyRegex = unsafeRegex "^(.|\\n)*[^(\\s)|(\\t)|(\\n)](.|\\n)*$" noFlags
