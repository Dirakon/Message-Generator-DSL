module Assertions where

import Prelude
import Types

import Data.Array (cons, filter, filterA, find, foldM, foldMap, head, index, partition, uncons)
import Data.Either (Either(..))
import Data.List (List, any, foldl)
import Data.Map (unionWith)
import Data.Map as Map
import Data.Map.Internal (keys)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Traversable (sequence, traverse)
import Data.Validation.Semigroup (V(..), invalid)
import Evaluate (tryEvaluateExpression)

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

  {unifiedDeclarations,otherVarDecs} = unifyVariableDeclarationsOnCondition variables variablesAreRelevant
  assertedUnifiedRelevantVarDecs = filter assertionIsTrue unifiedDeclarations

  -- TODO: add error on non-deterministic expression comparison
  assertionIsTrue determinedVariables =
    fromMaybe false
      $ do
          evaluatedExpr1 <- tryEvaluateExpression determinedVariables macros expr1
          evaluatedExpr2 <- tryEvaluateExpression determinedVariables macros expr2
          deteminedExpr1 <- head evaluatedExpr1
          deteminedExpr2 <- head evaluatedExpr2
          pure $ compareEvaluatedExpressions assertionType deteminedExpr1 deteminedExpr2

compareEvaluatedExpressions :: AssertionType -> DeterministicEvaluatedExpression -> DeterministicEvaluatedExpression -> Boolean
compareEvaluatedExpressions ExpressionsEqual a b = a == b

compareEvaluatedExpressions ExpressionsDifferent a b = a /= b


unifyVariableDeclarationsOnCondition :: Array NonDeterministicVariableDeclaration -> (NonDeterministicVariableDeclaration -> Boolean) 
  -> {unifiedDeclarations::NonDeterministicVariableDeclaration,otherVarDecs::Array NonDeterministicVariableDeclaration}
unifyVariableDeclarationsOnCondition varDecs condition = {unifiedDeclarations, otherVarDecs}
  where
    
  { yes: relevantVarDecs, no: otherVarDecs } = partition condition varDecs

  unifiedDeclarations = case uncons relevantVarDecs of
    Nothing -> []
    Just { head: initialState, tail: otherRelevantVarDecs } -> foldl unifyTwoVariableDeclarations initialState otherRelevantVarDecs


unifyTwoVariableDeclarations :: NonDeterministicVariableDeclaration -> NonDeterministicVariableDeclaration -> NonDeterministicVariableDeclaration
unifyTwoVariableDeclarations deterministicDecs1 deterministicDecs2 = unifiedDecs
  where
  unifiedDecs = do
    values1 <- deterministicDecs1
    values2 <- deterministicDecs2
    pure $ unionWith (\obj1 obj2 -> obj1) values1 values2
