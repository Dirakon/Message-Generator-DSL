module Evaluate where

import Prelude

import Data.AddressBook.Validation (Errors)
import Data.Either (Either)
import Data.Lazy (Lazy)
import Data.List.Lazy (List, nil)
import Data.Validation.Semigroup (V)
import Main (Expression)

evaluate :: Expression -> V Errors (List String)
evaluate expression = pure nil




