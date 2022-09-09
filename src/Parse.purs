module Parse where

import Prelude
import Data.Array (fromFoldable, length)
import Data.Array.NonEmpty (toArray)
import Data.List (List(..))
import Data.Map (empty)
import Data.Maybe (Maybe(..))
import Data.String (contains, joinWith, trim)
import Data.String.Regex (Regex, match, replace)
import Data.String.Regex (Regex, split)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.String.Utils (startsWith)
import Regex (assertionOperationRegex, doubleQuoteBodyGlobal)
import Types (BotState, Expression(..), ExpressionType, StatementType(..))

match' :: Regex -> String -> Array (Maybe String)
match' regex string = case match regex string of
  Nothing -> []
  Just arr -> toArray arr

-- TODO: Different return
parse :: String -> BotState
parse code =
  { assertions: []
  , macros: empty
  , variableConstructors: empty
  , variables: empty
  }

hasPatternOutsideQuotes :: String -> Regex -> Boolean
hasPatternOutsideQuotes text regex = 0 /= (length $ match' regex text)

removeDoubleQuotes :: String -> String
removeDoubleQuotes text = replace (doubleQuoteBodyGlobal) "" text

extractOneStatment ::
  List String ->
  Maybe
    { statementType :: StatementType
    , statementBody :: String
    , otherLines :: List String
    }
extractOneStatment Nil = Nothing

extractOneStatment (Cons line1 others) = case maybeStatementType of
  Just statementType -> Just { statementBody, statementType, otherLines }
  Nothing -> Nothing
  where
  analyzeLines Nil = { relatedLines: Nil, otherLines: Nil }

  analyzeLines (Cons curLine xs)
    | startsWith "\t" curLine = { relatedLines: (Cons curLine relatedLines'), otherLines: otherLines' }
      where
      { relatedLines: relatedLines', otherLines: otherLines' } = analyzeLines xs
    | otherwise = { relatedLines: Nil, otherLines: (Cons curLine xs) }

  { relatedLines, otherLines } = analyzeLines others

  statementBody = trim $ joinWith "" (fromFoldable (Cons line1 relatedLines))

  isAssertion = statementBody `hasPatternOutsideQuotes` assertionOperationRegex

  maybeStatementType
    | isAssertion = Just AssertionStatement
    | startsWith "#" statementBody = Just MacroStatement
    | startsWith "$" statementBody = Just VariableStatement
    | startsWith "[" statementBody = Just VariableStatement
    | otherwise = Nothing
