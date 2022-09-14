module Parse where

import Prelude

import Data.Array (elem, fromFoldable, length, toUnfoldable)
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.List (List(..), (:))
import Data.List (List(..), filter)
import Data.List.Types (List(..))
import Data.Map (empty)
import Data.Maybe (Maybe(..))
import Data.String (contains, joinWith, trim)
import Data.String.CodeUnits (fromCharArray, slice, toCharArray)
import Data.String.Regex (Regex, match, replace)
import Data.String.Regex (Regex, split)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.String.Utils (startsWith)
import Regex (assertionOperationRegex, doubleQuoteBodyGlobal)
import Tokenization (readUntilFirstOccurance, readUntilFirstOccuranceWithState, tokenize)
import Types (Assertion(..), AssertionType(..), Assigment(..), BotState, Expression(..), ExpressionType(..), Signature(..), Statement(..), Token(..))

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

extractOneStatement ::
  List String ->
  Maybe
    { statement :: Statement
    , otherLines :: List String
    }
extractOneStatement Nil = Nothing

extractOneStatement (line1 : others) = case parsedStatement of
  Just statement -> Just { statement, otherLines }
  Nothing -> Nothing
  where
  analyzeLines Nil = { relatedLines: Nil, otherLines: Nil }

  analyzeLines (curLine : xs)
    | startsWith "\t" curLine = { relatedLines: (curLine : relatedLines'), otherLines: otherLines' }
      where
      { relatedLines: relatedLines', otherLines: otherLines' } = analyzeLines xs
    | otherwise = { relatedLines: Nil, otherLines: (curLine : xs) }

  { relatedLines, otherLines } = analyzeLines others

  statementBody = trim $ joinWith "" (fromFoldable (line1 : relatedLines))

  parsedStatement = (tokensToStatement <<< tokenize <<< toUnfoldable <<< toCharArray) statementBody

tokensToStatement :: List Token -> Maybe Statement
tokensToStatement tokens = case assertionAnalysis of
  Just assertionInfo -> map AssertionStatement (parseAssertion assertionInfo)
  Nothing -> case assigmentAnalysis of
    Just assigmentInfo -> map AssigmentStatement (parseAssigment assigmentInfo)
    Nothing -> Nothing -- TODO: error, unknown type of expression
  where
  assertionAnalysis :: Maybe { preAssertion :: List Token, postAssertion :: List Token, assertionType :: AssertionType }
  assertionAnalysis = case tokens `splitByToken` AssertionEqualToken of
    Just x -> Just (toRightFormat x ExpressionsEqual)
    Nothing -> case tokens `splitByToken` AssertionDifferentToken of
      Just x -> Just (toRightFormat x ExpressionsDifferent)
      Nothing -> Nothing
    where
    toRightFormat { pre, post } assertionType = { preAssertion: pre, postAssertion: post, assertionType }

  assigmentAnalysis :: Maybe { preAssigment :: List Token, postAssigment :: List Token }
  assigmentAnalysis = map toRightFormat $ tokens `splitByToken` AssigmentToken
    where
    toRightFormat { pre, post } = { preAssigment: pre, postAssigment: post }

splitByToken :: List Token -> Token -> Maybe { pre :: List Token, post :: List Token }
splitByToken tokens tokenSplitter = map toRightFormat $ readUntilFirstOccurance tokens tokenSplitterIsMet
  where
  toRightFormat { readData, unreadData } = { pre: filter (_ /= tokenSplitter) readData, post: unreadData }

  tokenSplitterIsMet Nil = Nothing

  tokenSplitterIsMet (curToken : xs)
    | tokenSplitter == curToken = Just { moveRightAmount: 1 }
    | otherwise = Nothing

parseAssertion :: { preAssertion :: List Token, postAssertion :: List Token, assertionType :: AssertionType } -> Maybe Assertion
parseAssertion { preAssertion, postAssertion, assertionType } = ado
  expr1 <- parseExpression Nothing preAssertion
  expr2 <- parseExpression Nothing postAssertion
  in (Assertion assertionType expr1 expr2)

parseAssigment :: { preAssigment :: List Token, postAssigment :: List Token } -> Maybe Assigment
parseAssigment { preAssigment, postAssigment } = ado
  signature <- parseSignature preAssigment
  expr <- parseExpression Nothing postAssigment
  in (Assigment signature expr)

parseExpression :: Maybe Expression -> List Token -> Maybe Expression
parseExpression _ (TupleOpenedToken : xs) = Nothing -- TODO

parseExpression (Just operand1) (OneOfToken : xs) = Nothing

parseExpression _ _ = Nothing

-- TODO: add ALL actors post-factum (impossible to do with current arguments in the process)
extractOneExpression :: List Token -> Maybe { extractedExpression :: Expression, otherTokens :: List Token }
extractOneExpression ((LiteralToken literalValue) : otherTokens) = tryFindBinaryExpression constructedLiteral otherTokens
  where
  constructedLiteral = Expression (Literal literalValue) []

extractOneExpression ((VariableToken variableName) : otherTokens) =  tryFindBinaryExpression constructedVarialeCall otherTokens
  where
  constructedVarialeCall = Expression (VariableCall variableName) []

extractOneExpression ((MacroToken macroName) : otherTokens) = tryFindBinaryExpression constructedMacroCall otherTokens
  where
  constructedMacroCall = Expression (MacroCall macroName) []

extractOneExpression (TupleOpenedToken : otherTokens) = process otherTokens []
  where
  process :: List Token -> Array Expression -> Maybe { extractedExpression :: Expression, otherTokens :: List Token }
  process (TupleClosedToken : otherTokens) alreadyExtractedOnes = Just { extractedExpression, otherTokens }
    where
    extractedExpression = Expression (TupleOf alreadyExtractedOnes) []

  process (TupleNextItemToken : otherTokens) alreadyExtractedOnes = process otherTokens alreadyExtractedOnes

  process Nil _ = Nothing -- TODO: tuple not closed error

  process something alreadyExtractedOnes = Nothing --TODO! ado

-- map toRightFormat $ extractOneExpression something
-- where
-- toRightFormat { extractedExpression, otherTokens } = process otherTokens ([ extractedExpression ] <> alreadyExtractedOnes)
extractOneExpression Nil = Nothing

extractOneExpression _ = Nothing -- TODO: error, unexpected token

tryFindBinaryExpression :: Expression -> List Token -> Maybe { extractedExpression :: Expression, otherTokens :: List Token }
tryFindBinaryExpression expr1 Nil = Just { extractedExpression: expr1, otherTokens: Nil }

tryFindBinaryExpression expr1 (OneOfToken : xs) = ado
  { extractedExpression: expr2, otherTokens } <- extractOneExpression xs
  in { extractedExpression: Expression (OneOf expr1 expr2) [], otherTokens }

tryFindBinaryExpression expr1 (ConsolidationOfToken : xs) = ado
  { extractedExpression: expr2, otherTokens } <- extractOneExpression xs
  in { extractedExpression: Expression (ConsolidationOf expr1 expr2) [], otherTokens }

tryFindBinaryExpression expr1 _ = Nothing -- TODO: error, unexpected token

parseSignature :: List Token -> Maybe Signature
parseSignature (TupleOpenedToken : xs) = toSignature xs []
  where
  toSignature (Nil) readyArray = Nothing -- TODO: error, incomplete variable tuple assigment (can't find closing bracket)

  toSignature (TupleClosedToken : Nil) readyArray = Just $ VariableSignature readyArray

  toSignature ((VariableToken varName) : xs) readyArray = toSignature xs (readyArray <> [ varName ])

  toSignature (_ : xs) readyArray = toSignature xs readyArray

parseSignature ((VariableToken varName) : Nil) = Just (VariableSignature [ varName ])

parseSignature ((MacroToken macroName) : Nil) = Just (MacroSignature macroName)

parseSignature _ = Nothing



data IndentationCommand = IndentationChange Int
data SplitControlCommand = StopSplitting | ContinueSplitting | Cut
-- | splits a part of arbitrary list into array of lists based on indentation controlling and split controllng functions
indentationBasedSplitting :: forall a. List a -> (a -> IndentationCommand) -> (a -> SplitControlCommand) -> Maybe {splitPart::(Array (List a)), unreadPart:: (List a)}
indentationBasedSplitting list indentationCommand splitControlCommand = toRightFormat $ process Nil [] list indentationCommand splitControlCommand 
  where
    toRightFormat = identity
    process curpart previousParts Nil indentationCommand splitControlCommand =Nothing -- TODO
    
