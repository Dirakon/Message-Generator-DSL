module Parse where

import Prelude

import Data.Array (fromFoldable, length, toUnfoldable)
import Data.Array.NonEmpty (toArray)
import Data.List (filter, (:))
import Data.List.Types (List(..))
import Data.Maybe (Maybe(..))
import Data.String (joinWith, trim)
import Data.String.CodeUnits (toCharArray)
import Data.String.Regex (Regex, match, replace, split)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.String.Utils (startsWith)
import Data.Traversable (sequence)
import Regex (doubleQuoteBodyGlobal)
import Tokenization (readUntilFirstOccurance, tokenize)
import Types (Assertion(..), AssertionType(..), Assigment(..), Expression(..), ExpressionType(..), Signature(..), Statement(..), Token(..))

match' :: Regex -> String -> Array (Maybe String)
match' regex string = case match regex string of
  Nothing -> []
  Just arr -> toArray arr

-- TODO: Different return
parse :: String -> Boolean
parse code = true
  where
  statements = map instantiateActors actorlessStatements
  actorlessStatements = ((split (unsafeRegex "\n" noFlags)) >>> toUnfoldable >>> extractAllStatements) code

extractAllStatements :: List String -> List Statement
extractAllStatements lines = case extractOneStatement lines of
  Nothing -> Nil
  Just {statement, otherLines} -> statement : extractAllStatements otherLines

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
parseAssertion { preAssertion, postAssertion, assertionType } = do
  { extractedExpression: expr1 } <- extractOneExpression preAssertion
  { extractedExpression: expr2 } <- extractOneExpression postAssertion
  pure (Assertion assertionType expr1 expr2)

parseAssigment :: { preAssigment :: List Token, postAssigment :: List Token } -> Maybe Assigment
parseAssigment { preAssigment, postAssigment } = ado
  signature <- parseSignature preAssigment
  { extractedExpression: expr } <- extractOneExpression postAssigment
  in (Assigment signature expr)

-- TODO: add ALL actors post-factum (impossible to do with current arguments in the process)
extractOneExpression :: List Token -> Maybe { extractedExpression :: Expression, otherTokens :: List Token }
extractOneExpression ((LiteralToken literalValue) : otherTokens) = tryFindBinaryExpression constructedLiteral otherTokens
  where
  constructedLiteral = Expression (Literal literalValue) []

extractOneExpression ((VariableToken variableName) : otherTokens) = tryFindBinaryExpression constructedVarialeCall otherTokens
  where
  constructedVarialeCall = Expression (VariableCall variableName) []

extractOneExpression ((MacroToken macroName) : otherTokens) = tryFindBinaryExpression constructedMacroCall otherTokens
  where
  constructedMacroCall = Expression (MacroCall macroName) []

extractOneExpression (TupleOpenedToken : otherTokens) = case maybeConstructedTuple of
  Nothing -> Nothing
  Just { otherTokens, extractedExpression } -> tryFindBinaryExpression extractedExpression otherTokens
  where
  maybeConstructedTuple = case indentationBasedSplitting otherTokens indentationFunc of
    Nothing -> Nothing
    Just { splitPart, unreadPart } -> map (toRightFormat unreadPart) (parseSubExpressions splitPart)

  indentationFunc 0 TupleClosedToken = { controlCommand: StopSplitting, indentationChange: 0 }

  indentationFunc _ TupleClosedToken = { controlCommand: ContinueSplitting, indentationChange: -1 }

  indentationFunc _ TupleOpenedToken = { controlCommand: ContinueSplitting, indentationChange: 1 }

  indentationFunc 0 TupleNextItemToken = { controlCommand: CommenseSplitting, indentationChange: 0 }

  indentationFunc _ _ = { controlCommand: ContinueSplitting, indentationChange: 0 }

  parseSubExpressions subExpressions = sequence $ map (\subExpr -> ignoreOthers $ extractOneExpression subExpr) subExpressions

  ignoreOthers = map (\{ extractedExpression } -> extractedExpression)

  toRightFormat otherTokens subExpressions = { otherTokens, extractedExpression: Expression (TupleOf subExpressions) [] }

extractOneExpression (BraceOpenedToken : otherTokens) = case maybeConstructedBraces of
  Nothing -> Nothing
  Just { otherTokens, extractedExpression } -> tryFindBinaryExpression extractedExpression otherTokens
  where
  maybeConstructedBraces = case indentationBasedSplitting otherTokens indentationFunc of
    Nothing -> Nothing
    Just { splitPart, unreadPart } -> map (toRightFormat unreadPart) (parseSubExpression splitPart)

  indentationFunc 0 BraceClosedToken = { controlCommand: StopSplitting, indentationChange: 0 }

  indentationFunc _ BraceClosedToken = { controlCommand: ContinueSplitting, indentationChange: -1 }

  indentationFunc _ BraceOpenedToken = { controlCommand: ContinueSplitting, indentationChange: 1 }

  indentationFunc _ _ = { controlCommand: ContinueSplitting, indentationChange: 0 }

  parseSubExpression [ subExpression ] = ignoreOthers $ extractOneExpression subExpression

  parseSubExpression _ = Nothing -- TODO: error, should not happen, means that indentationBasedSplitting is broken

  ignoreOthers = map (\{ extractedExpression } -> extractedExpression)

  toRightFormat otherTokens subExpression = { otherTokens, extractedExpression: subExpression }

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

type IndentationSplittingControl
  = { indentationChange :: Int
    , controlCommand :: SplitControlCommand
    }

data SplitControlCommand
  = StopSplitting
  | ContinueSplitting
  | CommenseSplitting

-- | splits a part of arbitrary list into array of lists based on indentation controlling and split controllng functions
indentationBasedSplitting :: forall a. List a -> (Int -> a -> IndentationSplittingControl) -> Maybe { splitPart :: (Array (List a)), unreadPart :: (List a) }
indentationBasedSplitting list controlCommand = process Nil [] list 0
  where
  toRightFormat splitPart unreadPart = { splitPart, unreadPart }

  finallizeCurrentItem Nil arr = arr

  finallizeCurrentItem smth arr = arr <> [ smth ]

  process _ _ Nil _ = Nothing -- TODO Error, end of list reached, split is not stopped

  process partiallyAssembledPart assembledParts (el : xs) n = case controlCommand n el of
    { controlCommand: StopSplitting } -> Just $ toRightFormat (finallizeCurrentItem partiallyAssembledPart assembledParts) xs
    { indentationChange, controlCommand: ContinueSplitting } -> process (partiallyAssembledPart <> el : Nil) assembledParts xs (n + indentationChange)
    { indentationChange, controlCommand: CommenseSplitting } -> process Nil (finallizeCurrentItem partiallyAssembledPart assembledParts) xs (n + indentationChange)


instantiateActors :: Statement -> Statement
instantiateActors (AssigmentStatement (Assigment sign expr)) = AssigmentStatement (Assigment sign expr')
  where
  expr' = instantiateActorsInExpression expr
instantiateActors (AssertionStatement (Assertion asType expr1 expr2)) = (AssertionStatement (Assertion asType expr1' expr2'))
  where
  expr1' = instantiateActorsInExpression expr1
  expr2' = instantiateActorsInExpression expr2

instantiateActorsInExpression :: Expression -> Expression
instantiateActorsInExpression expr = expr -- TODO
instantiateActorsInExpression expr@(Expression (Literal _) _) = expr