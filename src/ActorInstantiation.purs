module ActorInstantiation where

import Prelude

import Data.Array (concatMap, fold, fromFoldable, length, nubByEq, toUnfoldable)
import Data.Array.NonEmpty (toArray)
import Data.List (all, elem, filter, mapMaybe, (:), partition)
import Data.List as L
import Data.List.Types (List(..))
import Data.Map (Map, empty, keys, lookup, unionWith, values)
import Data.Map.Internal as MI
import Data.Maybe (Maybe(..))
import Data.String (joinWith, trim)
import Data.String.CodeUnits (toCharArray)
import Data.String.Regex (Regex, match, replace, split)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.String.Utils (startsWith)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Debug (trace)
import Partial.Unsafe (unsafePartial)
import Regex (doubleQuoteBodyGlobal)
import Tokenization (readUntilFirstOccurance, tokenize)
import Types (ActorList, Assertion(..), AssertionType(..), Assigment(..), Expression(..), ExpressionType(..), Signature(..), Statement(..), Token(..))

variableActorPrefix ∷ String
variableActorPrefix = "!"

macroActorPrefix ∷ String
macroActorPrefix = ","

instantiateAllActors :: List Statement -> List Statement
instantiateAllActors initialStatements = instantiatedStatements
  where
  statementsWithLiteralActors = map (instantiateLiteralActorsInStatement) initialStatements

  allAssigments =
    mapMaybe
      ( case _ of
          AssigmentStatement assigment -> Just assigment
          _ -> Nothing
      )
      statementsWithLiteralActors

  allAssertions =
    mapMaybe
      ( case _ of
          AssertionStatement assertion -> Just assertion
          _ -> Nothing
      )
      statementsWithLiteralActors
    
  (Tuple instantiatedAssigments replacementTable) = instantiateAllActorsInAssigments allAssigments Nil empty
  instantiatedAssertions = instantiateAllActorsInAssertions allAssertions replacementTable

  instantiatedStatements = map (AssertionStatement) instantiatedAssertions <> map (AssigmentStatement) instantiatedAssigments


unsafeJust :: forall a. Maybe a -> a
unsafeJust =
  unsafePartial
    $ case _ of
        Just a -> a

instantiateAllActorsInAssertions :: List Assertion -> Map String (Array String) -> List Assertion
instantiateAllActorsInAssertions assertions replacementTable = map replaceActors assertions
  where
    replaceActorList oldActorList =  concatMap (\actorName -> unsafeJust $ lookup actorName replacementTable) oldActorList
    replaceActors (Assertion assertionType expr1 expr2) = Assertion assertionType expr1' expr2'
      where
        expr1' = recursivelyReplaceActors replaceActorList expr1
        expr2' = recursivelyReplaceActors replaceActorList expr2

instantiateAllActorsInAssigments :: List Assigment -> List Assigment -> Map String (Array String) -> Tuple (List Assigment) (Map String (Array String))
instantiateAllActorsInAssigments Nil instantiatedAssigments replacementTable = 
  Tuple instantiatedAssigments replacementTable

instantiateAllActorsInAssigments uninstantiatedAssigments instantiatedAssigments replacementTable =  instantiateAllActorsInAssigments uninstantiatableAssigments instantiatedAssigments' replacementTable'
  where
  replacablePseudoActors = keys replacementTable

  canInstantiate (Assigment _ (Expression _ actors)) = all (_ `elem` replacablePseudoActors) actors

  { yes: instantiatableAssigments, no: uninstantiatableAssigments } = partition canInstantiate uninstantiatedAssigments

  replacementFunction oldActorList = concatMap (\actorName -> unsafeJust $ lookup actorName replacementTable) oldActorList

  recursivelyReplaceActors' (Assigment signature expr) = Assigment signature (recursivelyReplaceActors replacementFunction expr)

  newInstantiatedAssigments = map recursivelyReplaceActors' instantiatableAssigments

  instantiatedAssigments' = newInstantiatedAssigments <> instantiatedAssigments

  getReplacements :: Assigment -> List (Tuple String (Array String))
  getReplacements (Assigment (MacroSignature macroName) (Expression _ actorList)) = (Tuple (macroActorPrefix <> macroName) (actorList)) : Nil
  
  getReplacements (Assigment (VariableSignature varNames) (Expression _ actorList)) =
    L.fromFoldable
      $ map (\varName -> Tuple (variableActorPrefix <> varName) (actorList <> [varName])) varNames

  newPartOfReplacementTable :: Map String (Array String)
  newPartOfReplacementTable = MI.fromFoldable $ L.concatMap getReplacements newInstantiatedAssigments

  replacementTable' = unionWith (\obj1 obj2 -> obj1) replacementTable newPartOfReplacementTable

--TODO
instantiateLiteralActorsInStatement :: Statement -> Statement
instantiateLiteralActorsInStatement (AssigmentStatement (Assigment sign expr)) = AssigmentStatement (Assigment sign expr')
  where
  expr' = instantiateLiteralActors expr

instantiateLiteralActorsInStatement (AssertionStatement (Assertion asType expr1 expr2)) = (AssertionStatement (Assertion asType expr1' expr2'))
  where
  expr1' = instantiateLiteralActors expr1

  expr2' = instantiateLiteralActors expr2

instantiateLiteralActors :: Expression -> Expression
instantiateLiteralActors initialExpr@(Expression exprType actors) = case exprType of
  OneOf expr1 expr2 -> Expression (OneOf expr1' expr2') allActors
    where
    expr1' = rec expr1

    expr2' = rec expr2

    allActors = addActors [ expr1', expr2' ]
  ConsolidationOf expr1 expr2 -> Expression (ConsolidationOf expr1' expr2') allActors
    where
    expr1' = rec expr1

    expr2' = rec expr2

    allActors = addActors [ expr1', expr2' ]
  Literal _ -> initialExpr
  MacroCall macroName -> Expression (MacroCall macroName) [ macroActorPrefix <> macroName ]
  VariableCall varName -> Expression (VariableCall varName) [ variableActorPrefix <> varName ]
  TupleOf exprs -> Expression (TupleOf exprs') allActors
    where
    exprs' = map rec exprs

    allActors = addActors exprs'
  where
  rec = instantiateLiteralActors

  extractActors (Expression _ actors') = actors'

  addActors expressions = nubByEq eq $ fold (map extractActors expressions)

recursivelyReplaceActors :: (ActorList -> ActorList) -> Expression -> Expression
recursivelyReplaceActors replaceActors initialExpr@(Expression exprType actors) = case exprType of
  OneOf expr1 expr2 -> Expression (OneOf expr1' expr2') allActors
    where
    expr1' = rec expr1

    expr2' = rec expr2

    allActors = addActors [ expr1', expr2' ]
  ConsolidationOf expr1 expr2 -> Expression (ConsolidationOf expr1' expr2') allActors
    where
    expr1' = rec expr1

    expr2' = rec expr2

    allActors = addActors [ expr1', expr2' ]
  Literal _ -> initialExpr
  MacroCall macroName -> Expression (MacroCall macroName) (replaceActors actors)
  VariableCall varName -> Expression (VariableCall varName) (replaceActors actors)
  TupleOf exprs -> Expression (TupleOf exprs') allActors
    where
    exprs' = map rec exprs

    allActors = addActors exprs'
  where
  rec = recursivelyReplaceActors replaceActors

  extractActors (Expression _ actors) = actors

  addActors expressions = nubByEq eq $ fold (map extractActors expressions)

