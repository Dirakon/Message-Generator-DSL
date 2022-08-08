module Types where

import Prelude

import Data.Array (head)
import Data.List (List)
import Data.Map (Map, empty)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)

type Errors
  = Array String

type MacroName
  = String

type VariableName
  = String

data Expression 
  = Expression ExpressionType ActorList

data ExpressionType
-- TODO: OneOf (+ConsolidationOf?) are arrayed
  = OneOf Expression Expression
  | ConsolidationOf Expression Expression
  | TupleOf (Array Expression)
  | MacroCall MacroName
  | VariableCall VariableName
  | Literal String

data Assigment
  = Assigment Signature Expression

data Assertion
  = Assertion AssertionType Expression Expression

data AssertionType
  = ExpressionsEqual
  | ExpressionsDifferent  

data Signature
  = MacroSignature MacroName
  | VariableSignature (Array VariableName)

-- | List of variables needed to compute an expression
type ActorList = Array VariableName

type EvaluatedExpression = Array StringTuple
type StringTuple = Array String

type MacroList = Map MacroName Expression
type VariableList = Map VariableName NonDeterministicVariableDeclaration
type VariableConstructorList = Map VariableName VariableConstructor


type BotState
  = { variables :: VariableList
    , variableConstructors :: VariableConstructorList
    , macros :: MacroList
    , assertions :: Array Assertion
    }


type DeterministicVariableDeclaration 
  = Map VariableName StringTuple

-- | ASSUMPTION: all deterministic declaration share the same set of variables 
type NonDeterministicVariableDeclaration 
  = Array DeterministicVariableDeclaration 


data VariableConstructor
  = VariableConstructor (Array VariableName) Expression



singletonNonDeterministicDeclaration :: DeterministicVariableDeclaration -> NonDeterministicVariableDeclaration
singletonNonDeterministicDeclaration dec = [dec]

singletonVariableList :: NonDeterministicVariableDeclaration -> VariableList
singletonVariableList deterministicDecs = case head deterministicDecs of
  Nothing -> empty
  Just deterministicDec -> map (\_ -> deterministicDecs) deterministicDec