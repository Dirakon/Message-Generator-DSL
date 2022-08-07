module Types where

import Prelude
import Data.Map (Map)
import Data.Tuple (Tuple)

type Errors
  = Array String

type MacroName
  = String

type VariableName
  = String

data Expression
-- TODO: OneOf (+ConsolidationOf?) are arrayed
  = OneOf Expression Expression
  | ConsolidationOf Expression Expression
  | UnionOf (Array Expression)
  | MacroCall MacroName
  | VariableCall VariableName
  | Literal String

data Assigment
  = Assigment Signature Expression

data Assertion
  = Assertion AssertionType (Array VariableName) Expression Expression

data AssertionType
  = ExpressionsEqual
  | ExpressionsDifferent  

data Signature
  = MacroSignature MacroName
  | VariableSignature (Array VariableName)

type BotState
  = { variables :: Map VariableName String
    , variableConstructors :: Map VariableName Expression
    , macros :: Map MacroName Expression
    , assertions :: Array Assertion
    }
