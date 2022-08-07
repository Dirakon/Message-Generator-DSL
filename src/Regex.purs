module Regex where

import Prelude

import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)


notPreceededBySlashRegex :: String -> Regex
notPreceededBySlashRegex patten = unsafeRegex ("[^\\\\]\\" <> patten <> "") noFlags


-- stateRegex :: Regex
-- stateRegex = unsafeRegex "^[A-Z | a-z][A-Z | a-z]$" noFlags
-- nonEmptyRegex :: Regex
-- nonEmptyRegex = unsafeRegex "^(.|\\n)*[^(\\s)|(\\t)|(\\n)](.|\\n)*$" noFlags
