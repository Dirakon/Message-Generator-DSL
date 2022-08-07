module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Test.Parse (regexTests)
import Test.Unit (TestF, test)
import Test.Unit (suite)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main =
  runTest do
    regexTests
        
        