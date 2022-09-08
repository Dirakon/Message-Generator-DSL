module Format where

import Prelude
import Data.Array (filter)
import Data.String (Pattern(..), joinWith, length, split)
import Data.String.Utils (startsWith)

format :: String -> String
format = (removeEmptyLines >>> removeComments >>> markInquotesSpecialSymbols)

markInquotesSpecialSymbols :: String -> String
markInquotesSpecialSymbols = identity

removeComments :: String -> String
removeComments original = joinWith "\n" filteredLines
  where
  lines = split (Pattern "\n") original

  filteredLines = filter (\line -> not $ startsWith ">>>" line) lines

removeEmptyLines :: String -> String
removeEmptyLines original = joinWith "\n" filteredLines
  where
  lines = split (Pattern "\n") original

  filteredLines = filter (\line -> length line /= 0) lines
