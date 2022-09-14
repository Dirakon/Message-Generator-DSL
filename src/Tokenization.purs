module Tokenization where

import Prelude

import Data.Array (elem, fromFoldable)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray, slice)
import Data.Tuple (Tuple(..))
import Types (Token(..))

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
readUntilFirstOccurance elements occuranceFunc = 
  readUntilFirstOccuranceWithState elements funcBridge unit
  where
    funcBridge arg _ = case occuranceFunc arg of
      Nothing -> Tuple Nothing unit
      Just {moveRightAmount} -> Tuple (Just moveRightAmount) unit

readUntilFirstOccuranceWithState :: forall a b. List a -> (List a->b -> Tuple (Maybe Int) b) -> b -> Maybe { readData :: List a, unreadData :: List a }
readUntilFirstOccuranceWithState elements occuranceFunc curState = case occuranceFunc elements curState of
  Tuple (Just moveRightAmount) _ -> Just recursiveResults
    where
    recursiveResults = tryGrabElements elements moveRightAmount
  Tuple Nothing newState -> case elements of
    Nil -> Nothing -- TODO: error, occurance function could not find the matching element
    readChar : unreadElements -> recursiveResults
      where
      recursiveResults = case readUntilFirstOccuranceWithState unreadElements occuranceFunc newState of
        Nothing -> Nothing -- TODO: error, occurance function could not find the matching element
        Just { readData, unreadData } -> Just { readData: readChar : readData, unreadData }

tryGrabElements :: forall a. List a -> Int -> { readData :: List a, unreadData :: List a }
tryGrabElements = tryGrabElements' Nil
  where
  tryGrabElements' :: List a -> List a -> Int -> { readData :: List a, unreadData :: List a }
  tryGrabElements' readData unreadData 0 = { readData, unreadData }

  tryGrabElements' readData Nil _ = { readData, unreadData: Nil }

  tryGrabElements' readData (char : xs) n = tryGrabElements' (readData <> (char : Nil)) xs (n - 1)