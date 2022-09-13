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
import Types (BotState, Expression(..), ExpressionType(..), Token(..))

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

extractOneExpression ::
  List String ->
  Maybe
    { expression :: Expression
    , otherLines :: List String
    }
extractOneExpression Nil = Nothing

extractOneExpression (Cons line1 others) = case parsedExpression of
  Just expression -> Just { expression, otherLines }
  Nothing -> Nothing
  where
  analyzeLines Nil = { relatedLines: Nil, otherLines: Nil }

  analyzeLines (Cons curLine xs)
    | startsWith "\t" curLine = { relatedLines: (Cons curLine relatedLines'), otherLines: otherLines' }
      where
      { relatedLines: relatedLines', otherLines: otherLines' } = analyzeLines xs
    | otherwise = { relatedLines: Nil, otherLines: (Cons curLine xs) }

  { relatedLines, otherLines } = analyzeLines others

  statementBody = trim $ joinWith "" (fromFoldable (Cons line1 relatedLines))

  parsedExpression = tokensToExpression $ tokenize (toUnfoldable $ toCharArray statementBody)

tokenize :: List Char -> List Token
tokenize chars = case extractOneToken chars of
  Nothing -> Nil
  Just { extractedToken, otherChars } -> (Cons extractedToken (tokenize otherChars))

naturalTokenSpliters :: Array Char
naturalTokenSpliters = [ '\n', ' ', '\t' ]

allTokenSplitters :: Array Char
allTokenSplitters = naturalTokenSpliters <> [ '[', ']', ',', '|', '+', '!', '=' ]

extractOneToken :: List Char -> Maybe { extractedToken :: Token, otherChars :: List Char }
extractOneToken chars = case chars of
  Nil -> Nothing
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
    Nothing -> Nothing
    Just { readData, unreadData } -> finish (wrapperFunc readData) unreadData

  tokenSplitterIsMet Nil = Just { moveRightAmount: 0 }

  tokenSplitterIsMet (char : _) = if char `elem` allTokenSplitters then Just { moveRightAmount: 0 } else Nothing

  unslashedQuoteIsMet ('\\' : '"' : _) = Nothing

  unslashedQuoteIsMet ('"' : _) = Just { moveRightAmount: 1 }

  unslashedQuoteIsMet (_ : '"' : _) = Just { moveRightAmount: 2 }

  unslashedQuoteIsMet _ = Nothing

  removeLastChar = slice 0 (-1)

readUntilFirstOccurance :: List Char -> (List Char -> Maybe { moveRightAmount :: Int }) -> Maybe { readData :: String, unreadData :: List Char }
readUntilFirstOccurance chars occuranceFunc = case occuranceFunc chars of
  Just { moveRightAmount } -> Just recursiveResults
    where
    recursiveResults = tryGrabCharacters chars moveRightAmount
  Nothing -> case chars of
    Nil -> Nothing
    readChar : unreadChars -> recursiveResults
      where
      recursiveResults = case readUntilFirstOccurance unreadChars occuranceFunc of
        Nothing -> Nothing
        Just { readData, unreadData } -> Just { readData: fromCharArray [ readChar ] <> readData, unreadData }

tryGrabCharacters :: List Char -> Int -> { readData :: String, unreadData :: List Char }
tryGrabCharacters = tryGrabCharacters' ""
  where
  tryGrabCharacters' readData unreadData 0 = { readData, unreadData }

  tryGrabCharacters' readData Nil _ = { readData, unreadData: Nil }

  tryGrabCharacters' readData (char : xs) n = tryGrabCharacters' (readData <> fromCharArray [ char ]) xs (n - 1)

tokensToExpression :: List Token -> Maybe Expression
tokensToExpression l = Nothing
