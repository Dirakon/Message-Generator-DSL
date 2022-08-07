module Format where

import Prelude

format :: String -> String
format = (markInquotesSpecialSymbols >>> removeComments)

markInquotesSpecialSymbols :: String -> String
markInquotesSpecialSymbols = identity

removeComments :: String -> String
removeComments = identity