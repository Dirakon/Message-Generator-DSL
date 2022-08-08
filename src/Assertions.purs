module Assertions where

import Prelude
import Types

import Data.Array (filterA)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (fromMaybe)
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
assertionHolds :: Assertion -> MacroList -> DeterministicVariableDeclaration -> V Errors Boolean
assertionHolds (Assertion assertionType leftSide rightSide) macros variables =
  fromMaybe (pure true)
    $ ado
        evaluatedLeft <- tryEvaluateExpression singletonVariableList' macros leftSide
        evaluatedRight <- tryEvaluateExpression singletonVariableList' macros rightSide
        in case [ evaluatedLeft, evaluatedRight ] of
         [ [ evaluatedTuple1 ], [ evaluatedTuple2 ] ] -> pure $ compareEvaluatedTuples assertionType evaluatedTuple1 evaluatedTuple2
         _ -> invalid [ "assertions cannot be non-deterministic" ]
  where
  singletonVariableList' = singletonVariableList (singletonNonDeterministicDeclaration variables)

compareEvaluatedTuples :: AssertionType -> StringTuple -> StringTuple -> Boolean
compareEvaluatedTuples ExpressionsEqual a b = a == b

compareEvaluatedTuples ExpressionsDifferent a b = a /= b

-- assertionDefined :: Assertion -> BotState -> Boolean
-- assertionDefined (Assertion _ conceredVariables _ _) state = all (\variable -> variableDefined variable state) conceredVariables
filterVariableDeclaration :: MacroList -> NonDeterministicVariableDeclaration -> Assertion -> V Errors NonDeterministicVariableDeclaration
filterVariableDeclaration macros varDeclarations assertion = ado
  filteredDeclarations <- filterA assertionHolds' varDeclarations
  in filteredDeclarations
    where
    assertionHolds' :: DeterministicVariableDeclaration -> V Errors Boolean
    assertionHolds' declaration 
      = assertionHolds assertion macros declaration
