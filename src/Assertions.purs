module Assertions where

import Prelude
import Types

import Data.Array (cons, filter, filterA, find, foldM, foldMap, foldl, head, index, partition, uncons)
import Data.Either (Either(..))
import Data.List (any, List)
import Data.Map as Map
import Data.Map.Internal (keys)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Traversable (sequence, traverse)
import Data.Validation.Semigroup (V(..), invalid)
import Evaluate (tryEvaluateExpression, unifyVariableDeclarations)

-- -- | Check every assertion with 'assertionHolds', accumulate erros if there are any.
-- assertionsHold :: BotState -> V Errors Boolean
-- assertionsHold state = ado
--   assertionsHold' <- sequence $ map (\assertion -> assertionHolds assertion state) state.assertions
--   in all identity assertionsHold'
-- | Return true if assertion is true or if it cannot be checked yet.
-- Throws error if assertion is invalid (non-deterministic, e.g. "$var == $delta | $gamma")

-- TODO: lift errors
applyAssertions :: Array Assertion -> MacroList -> NonDeterministicVariableList -> NonDeterministicVariableList
applyAssertions assertions macros variables = foldl (applySingleAssertion macros) variables assertions

applySingleAssertion :: MacroList -> NonDeterministicVariableList -> Assertion -> NonDeterministicVariableList
applySingleAssertion macros variables (Assertion assertionType expr1@(Expression _ actors1) expr2@(Expression _ actors2)) = 
  [ assertedUnifiedRelevantVarDecs ] <> otherVarDecs
  where
  allActors = actors1 <> actors2

  variablesAreRelevant :: NonDeterministicVariableDeclaration -> Boolean
  variablesAreRelevant varDec = case head varDec of
    Nothing -> false
    Just deterministicDeclaration -> containsRelevantActor (keys deterministicDeclaration)

  containsRelevantActor :: List VariableName -> Boolean
  containsRelevantActor =
    any
      ( \varName -> case find (_ == varName) allActors of
          Nothing -> false
          _ -> true
      )

  { yes: relevantVarDecs, no: otherVarDecs } = partition variablesAreRelevant variables

  unifiedRelevantVarDecs = case uncons relevantVarDecs of
    Nothing -> []
    Just{head:initialState, tail: otherRelevantVarDecs} -> foldl unifyVariableDeclarations initialState otherRelevantVarDecs

  assertedUnifiedRelevantVarDecs = filter assertionIsTrue unifiedRelevantVarDecs

  assertionIsTrue determinedVariables = fromMaybe false $
    do
      evaluatedExpr1 <- tryEvaluateExpression determinedVariables macros expr1
      evaluatedExpr2 <- tryEvaluateExpression determinedVariables macros expr2
      deteminedExpr1 <- head evaluatedExpr1
      deteminedExpr2 <- head evaluatedExpr2
      pure $ compareEvaluatedExpressions assertionType deteminedExpr1 deteminedExpr2

compareEvaluatedExpressions :: AssertionType -> DeterministicEvaluatedExpression -> DeterministicEvaluatedExpression -> Boolean
compareEvaluatedExpressions ExpressionsEqual a b = a == b
compareEvaluatedExpressions ExpressionsDifferent a b = a /= b
-- assertionHolds :: Assertion -> MacroList -> DeterministicVariableDeclaration -> V Errors Boolean
-- assertionHolds (Assertion assertionType leftSide rightSide) macros variables =
--   fromMaybe (pure true)
--     $ ado
--         evaluatedLeft <- tryEvaluateExpression singletonVariableList' macros leftSide
--         evaluatedRight <- tryEvaluateExpression singletonVariableList' macros rightSide
--         in case [ evaluatedLeft, evaluatedRight ] of
--          [ [ evaluatedTuple1 ], [ evaluatedTuple2 ] ] -> pure $ compareEvaluatedTuples assertionType evaluatedTuple1 evaluatedTuple2
--          _ -> invalid [ "assertions cannot be non-deterministic" ]
--   where
--   singletonVariableList' = singletonVariableList (singletonNonDeterministicDeclaration variables)
-- -- assertionDefined :: Assertion -> BotState -> Boolean
-- -- assertionDefined (Assertion _ conceredVariables _ _) state = all (\variable -> variableDefined variable state) conceredVariables
-- filterVariableDeclaration :: MacroList -> NonDeterministicVariableDeclaration -> Assertion -> V Errors NonDeterministicVariableDeclaration
-- filterVariableDeclaration macros varDeclarations assertion = ado
--   filteredDeclarations <- filterA assertionHolds' varDeclarations
--   in filteredDeclarations
--     where
--     assertionHolds' :: DeterministicVariableDeclaration -> V Errors Boolean
--     assertionHolds' declaration 
--       = assertionHolds assertion macros declaration
