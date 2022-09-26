module VariableInstantiation
  ( instantiateVariables
  ) where

import Prelude

import Assertions (applyAssertions, unifyVariableDeclarationsOnCondition)
import Data.Array as A
import Data.Foldable (all, any, elem)
import Data.List (List(..), partition, (:))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as S
import Data.Tuple (Tuple(..))
import Debug (trace)
import Evaluate (tryEvaluateExpression)
import Types (Assertion(..), Assigment(..), DeterministicEvaluatedExpression, DeterministicVariableDeclaration, Expression(..), MacroList, MacroName, NonDeterministicVariableDeclaration, Signature(..), Statement(..), VariableName, nonDeterministicVariableDeclaration)
import Utils (extractAssertions, extractMacros, extractVariableAssigments)

instantiateVariables :: List Statement -> Array NonDeterministicVariableDeclaration
instantiateVariables statements = instantiateVariablesInternal [] varAssgiments macros assertions
  where
  varAssgiments = extractVariableAssigments statements

  macros = extractMacros statements

  assertions = extractAssertions statements

instantiateVariablesInternal :: Array NonDeterministicVariableDeclaration -> List (Tuple (Array VariableName) Expression) -> MacroList -> List Assertion -> Array NonDeterministicVariableDeclaration
instantiateVariablesInternal initiatedVarDecs Nil _ Nil = initiatedVarDecs

instantiateVariablesInternal initiatedVarDecs unprocessedVarAssigments macros unusedAssertions =  case initiatableAssigments of
    L.Nil -> initiatedVarDecs
    _ -> instantiateVariablesInternal chipedAwayVarDecs uninitiatableAssigments macros unusedAssertions'
  where
  initiatedVars = A.concat $ A.mapMaybe getAllVariableNames initiatedVarDecs

  getAllVariableNames arr = case A.head arr of
    Nothing -> Nothing
    Just varToExpr -> Just $ A.fromFoldable $ M.keys varToExpr

  canInitiate (Tuple varSignature (Expression _ actors)) = A.all (_ `L.elem` initiatedVars) actors

  { yes: initiatableAssigments, no: uninitiatableAssigments } = partition canInitiate unprocessedVarAssigments

  processNeededVarDecs L.Nil varDecs = varDecs

  processNeededVarDecs ((Tuple varSignatures expr@(Expression _ requiredVars)) : xs) varDecs = processNeededVarDecs xs ([ unifiedDeclarations' ] <> otherVarDecs)
    where
    isVarDecRelated nonDetVarDec = case A.head nonDetVarDec of
      Nothing -> false
      Just varToExpr -> any (_ `A.elem` requiredVars) (M.keys varToExpr)

    { unifiedDeclarations, otherVarDecs } = unifyVariableDeclarationsOnCondition varDecs isVarDecRelated

    addNewVars :: DeterministicVariableDeclaration -> Array DeterministicVariableDeclaration
    addNewVars deterministicVarDeclaration = newMap
      where
      newVarsOnlyMap = nonDeterministicVariableDeclaration varSignatures possibleVarValues

      newMap = map (\determinisiticDeclaration -> M.unionWith (\obj1 obj2 -> obj1) deterministicVarDeclaration determinisiticDeclaration) newVarsOnlyMap

      possibleVarValues :: Array DeterministicEvaluatedExpression
      possibleVarValues =
        fromMaybe []
          $ ado
              evaluatedExpr <- tryEvaluateExpression deterministicVarDeclaration macros expr
              in evaluatedExpr

    -- TODO! Dangerouns, we might lose some values with bugs (unchecked)
    unifiedDeclarations' = case unifiedDeclarations of 
      [] -> calculateIndependentVariables
      _ ->A.concatMap addNewVars unifiedDeclarations
      where
        calculateIndependentVariables = case tryEvaluateExpression M.empty macros expr of
            Nothing ->  []
            Just varValues -> nonDeterministicVariableDeclaration varSignatures varValues

  initiatedVarDecs' = processNeededVarDecs initiatableAssigments initiatedVarDecs

  varIsInitiated varName = any varIsThere initiatedVarDecs'
    where
    varIsThere nonDetVarDec = case A.head nonDetVarDec of
      Nothing -> false
      Just varToExpr -> varName `elem` (M.keys varToExpr)

  isAsserionRelated (Assertion _ (Expression _ actors1) (Expression _ actors2)) =
    all varIsInitiated actors1
      && all varIsInitiated actors2

  { yes: completableAssertions, no: unusedAssertions' } = partition isAsserionRelated unusedAssertions

  chipedAwayVarDecs = applyAssertions (A.fromFoldable completableAssertions) macros initiatedVarDecs'
