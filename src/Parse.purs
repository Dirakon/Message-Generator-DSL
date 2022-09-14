module Parse where

import Prelude
import Data.Array (elem, fromFoldable, length, toUnfoldable)
import Data.Array.NonEmpty (toArray)
import Data.List (List(..))
import Data.List (List(..), (:))
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
import Types (Assertion(..), AssertionType, Assigment(..), BotState, Expression(..), ExpressionType(..), Signature, Statement(..), Token(..))

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

tokenize :: List Char -> List Token
tokenize chars = case extractOneToken chars of
  Nothing -> Nil
  Just { extractedToken, otherChars } -> extractedToken : (tokenize otherChars)

naturalTokenSpliters :: Array Char
naturalTokenSpliters = [ '\n', ' ', '\t' ]

allTokenSplitters :: Array Char
allTokenSplitters = naturalTokenSpliters <> [ '[', ']', ',', '|', '+', '!', '=' ]

extractOneToken :: List Char -> Maybe { extractedToken :: Token, otherChars :: List Char }
extractOneToken chars = case chars of
  Nil -> Nothing -- Nothing is the correct behaviour: no tokens were found
  '[' : xs -> finish TupleOpenedToken xs
  ']' : xs -> finish TupleClosedToken xs
  ',' : xs -> finish TupleNextItemToken xs
  '|' : xs -> finish OneOfToken xs
  '+' : xs -> finish ConsolidationOfToken xs
  '!' : '=' : xs -> finish AssertionDifferentToken xs
  '=' : '=' : xs -> finish AssertionEqualToken xs
  '=' : xs -> finish AssigmentToken xs
  '$' : xs -> finishWithOccuranceReader tokenSplitterIsMet xs (VariableToken)
  '#' : xs -> finishWithOccuranceReader tokenSplitterIsMet xs (MacroToken)
  '"' : xs -> finishWithOccuranceReader unslashedQuoteIsMet xs (LiteralToken <<< removeLastChar)
  char : xs -> if char `elem` naturalTokenSpliters then extractOneToken xs else Nothing -- TODO: error "unknown token beginning"
  where
  finish extractedToken otherChars = Just { otherChars, extractedToken }

  finishWithOccuranceReader occuranceFunc xs wrapperFunc = case readUntilFirstOccurance xs occuranceFunc of
    Nothing -> Nothing -- TODO: error, occurance function could not find the matching character
    Just { readData, unreadData } -> finish (wrapperFunc (fromCharArray $ fromFoldable readData)) unreadData

  tokenSplitterIsMet Nil = Just { moveRightAmount: 0 }

  tokenSplitterIsMet (char : _) = if char `elem` allTokenSplitters then Just { moveRightAmount: 0 } else Nothing

  unslashedQuoteIsMet ('\\' : '"' : _) = Nothing

  unslashedQuoteIsMet ('"' : _) = Just { moveRightAmount: 1 }

  unslashedQuoteIsMet (_ : '"' : _) = Just { moveRightAmount: 2 }

  unslashedQuoteIsMet _ = Nothing

  removeLastChar = slice 0 (-1)

readUntilFirstOccurance :: forall a. List a -> (List a -> Maybe { moveRightAmount :: Int }) -> Maybe { readData :: List a, unreadData :: List a }
readUntilFirstOccurance elements occuranceFunc = case occuranceFunc elements of
  Just { moveRightAmount } -> Just recursiveResults
    where
    recursiveResults = tryGrabElements elements moveRightAmount
  Nothing -> case elements of
    Nil -> Nothing -- TODO: error, occurance function could not find the matching character
    readChar : unreadElements -> recursiveResults
      where
      recursiveResults = case readUntilFirstOccurance unreadElements occuranceFunc of
        Nothing -> Nothing -- TODO: error, occurance function could not find the matching character
        Just { readData, unreadData } -> Just { readData: readChar : readData, unreadData }

tryGrabElements :: forall a. List a -> Int -> { readData :: List a, unreadData :: List a }
tryGrabElements = tryGrabElements' Nil
  where
  tryGrabElements' :: List a -> List a -> Int -> { readData :: List a, unreadData :: List a }
  tryGrabElements' readData unreadData 0 = { readData, unreadData }

  tryGrabElements' readData Nil _ = { readData, unreadData: Nil }

  tryGrabElements' readData (char : xs) n = tryGrabElements' (readData <> (char : Nil)) xs (n - 1)

tokensToStatement :: List Token -> Maybe Statement
tokensToStatement l = case assertionAnalysis of
  Just assertionInfo -> map AssertionStatement (parseAssertion assertionInfo)
  Nothing -> case assigmentAnalysis of
    Just assigmentInfo -> map AssigmentStatement (parseAssigment assigmentInfo)
    Nothing -> Nothing -- TODO: error, unknown type of expression
  where
  assertionAnalysis :: Maybe { preAssertion :: List Token, postAssertion :: List Token, assertionType :: AssertionType }
  assertionAnalysis = Nothing -- TODO

  assigmentAnalysis :: Maybe { preAssigment :: List Token, postAssigment :: List Token }
  assigmentAnalysis = Nothing -- TODO

parseAssertion :: { preAssertion :: List Token, postAssertion :: List Token, assertionType :: AssertionType } -> Maybe Assertion
parseAssertion { preAssertion, postAssertion, assertionType } = ado
  expr1 <- parseExpression preAssertion
  expr2 <- parseExpression postAssertion
  in (Assertion assertionType expr1 expr2)

parseAssigment :: { preAssigment :: List Token, postAssigment :: List Token } -> Maybe Assigment
parseAssigment { preAssigment, postAssigment } = Nothing -- TODO

parseExpression :: List Token -> Maybe Expression
parseExpression tokens = Nothing

parseSignature :: List Token -> Maybe Signature
parseSignature _ = Nothing
