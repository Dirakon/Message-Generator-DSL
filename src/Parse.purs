module Parse where

import Prelude

import Data.Array (fromFoldable)
import Data.List (List(..))
import Data.Map (empty)
import Data.Maybe (Maybe(..))
import Data.String (joinWith, trim)
import Data.String.Regex (split)
import Data.String.Utils (startsWith)
import Types (BotState, Expression(..), ExpressionType, StatementType(..))

-- TODO: Different return
parse :: String -> BotState
parse code =
  { assertions: []
  , macros: empty
  , variableConstructors: empty
  , variables: empty
  }

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

  isAssertion = false -- TODO: analyze statementBody for '==' or '!='

  maybeStatementType
    | isAssertion = Just AssertionStatement
    | startsWith "#" statementBody = Just MacroStatement
    | startsWith "$" statementBody = Just VariableStatement
    | startsWith "[" statementBody = Just VariableStatement
    | otherwise = Nothing