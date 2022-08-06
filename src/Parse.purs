module Parse where

import Prelude

import Main (Expression(..))

parse :: String -> Expression
parse string = Literal string