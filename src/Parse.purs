module Parse where

import Prelude
import Data.Map (empty)
import Data.String.Regex (split)
import Types (Expression(..), BotState)

parse :: String -> BotState
parse code =
  { assertions: []
  , macros: empty
  , variableConstructors: empty
  , variables: empty
  }
