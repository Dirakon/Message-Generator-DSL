module Test.Parse where

import Prelude
import Control.Monad.Free (Free)
import Data.Array (length)
import Data.Array.NonEmpty (toArray)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), replaceAll)
import Data.String.Regex (Regex, match, replace)
import Regex (doubleQuoteBodyGlobal, notPreceededBySlashRegex)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert
import Utils (match')

regexTests :: Free TestF Unit
regexTests =
  suite "regexTests" do
    notPreceededBySlashRegexTests
    doubleQuoteBodyTests

doubleQuoteBodyTests :: Free TestF Unit
doubleQuoteBodyTests =
  suite "doubleQuoteBodyTests" do
    test "Replaces all double quotes in text" do
      Assert.equal "Text free of double quotes"
        $ replace (doubleQuoteBodyGlobal) "" "Text\"NOT\" free of double quotes\"HAHA\""

notPreceededBySlashRegexTests :: Free TestF Unit
notPreceededBySlashRegexTests =
  suite "notPreceededBySlashRegex" do
    test "Does not find when preceeded by slash" do
      Assert.equal 0
        $ length (match' (notPreceededBySlashRegex "l") "dsa\\ldasd")
    test "Does find when not preceeded by slash" do
      Assert.equal 1
        $ length (match' (notPreceededBySlashRegex "l") "dsaldasd")
    test "Works with special symbols" do
      Assert.equal 1
        $ length (match' (notPreceededBySlashRegex "\\") "dsa\\ldasd")
      Assert.equal 0
        $ length (match' (notPreceededBySlashRegex ".") "dsa\\.ldasd")
      Assert.equal 1
        $ length (match' (notPreceededBySlashRegex "*") "dsa*ldasd")
