module Types where

import Prelude
import Data.Array (head, zip)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Map (Map, empty, fromFoldable, singleton)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))

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
type ActorList
  = Array VariableName

type NonDeterministicEvaluatedExpression
  = Array DeterministicEvaluatedExpression

data DeterministicEvaluatedExpression
  = TreeExpression EvaluatedExpressionContainer
  | LeafExpression String

type EvaluatedExpressionContainer
  = Array DeterministicEvaluatedExpression

derive instance genericDeterministicEvaluatedExpression :: Generic DeterministicEvaluatedExpression _

derive instance eqDeterministicEvaluatedExpression :: Eq DeterministicEvaluatedExpression

instance showDeterministicEvaluatedExpression :: Show DeterministicEvaluatedExpression where
  show s = genericShow s

type MacroList
  = Map MacroName Expression

type NonDeterministicVariableList
  = Array NonDeterministicVariableDeclaration

type VariableConstructorList
  = Map VariableName VariableConstructor

type BotState
  = { variables :: DeterministicVariableDeclaration
    , variableConstructors :: VariableConstructorList
    , macros :: MacroList
    , assertions :: Array Assertion
    }

type DeterministicVariableDeclaration
  = Map VariableName DeterministicEvaluatedExpression

-- | ASSUMPTION: all deterministic declaration share the same set of variables 
type NonDeterministicVariableDeclaration
  = Array DeterministicVariableDeclaration

data VariableConstructor
  = VariableConstructor (Array VariableName) Expression

singletonNonDeterministicDeclaration :: DeterministicVariableDeclaration -> NonDeterministicVariableDeclaration
singletonNonDeterministicDeclaration dec = [ dec ]

evaluatedExpressionToArray :: DeterministicEvaluatedExpression -> Array DeterministicEvaluatedExpression
evaluatedExpressionToArray (TreeExpression arr) = arr

evaluatedExpressionToArray leafExpression = [ leafExpression ]

nonDeterministicVariableDeclaration :: Array VariableName -> Array DeterministicEvaluatedExpression -> NonDeterministicVariableDeclaration
nonDeterministicVariableDeclaration [ singleVarName ] possibleValues = map makeOneMap possibleValues
  where
  makeOneMap (TreeExpression [ oneEl ]) = singleton singleVarName oneEl

  makeOneMap el = singleton singleVarName el

nonDeterministicVariableDeclaration multiVarList possibleValues = map makeOneMap possibleValues
  where
  makeOneMap (TreeExpression els) = fromFoldable (zip multiVarList els)

  makeOneMap el = makeOneMap (TreeExpression [ el ])

data Token
  = VariableToken String -- `$`
  | MacroToken String -- `#`
  | LiteralToken String -- `"`
  | OneOfToken -- `|`
  | ConsolidationOfToken -- `+`
  | TupleOpenedToken -- `[`
  | TupleClosedToken -- `]`
  | TupleNextItemToken -- `,`
  | AssertionEqualToken -- `==`
  | AssertionDifferentToken -- `!=`
  | AssigmentToken -- `=`