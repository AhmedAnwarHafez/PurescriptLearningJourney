module Ch2e where

import Ch2 (isWord)

import Data.Array (reverse)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Prelude

-- Check a word is plaindrome
-- "hannah" for example
isPalindrome :: String -> Boolean
isPalindrome word = 
    case isWord word of
        Nothing -> false
        -- reverse the word and check with itself
        Just _ -> (toCharArray word) == (reverse $ toCharArray word)


-- Leetspeak excercise
-- https://en.wikipedia.org/wiki/Leet 
-- The easiest way is translate a word is to convert a string into array of chars
-- then map transalateChar then convert it back to String type
translateWord :: String -> String
translateWord word = 
    fromCharArray $ map translateChar $ toCharArray word

-- A simple function that acts as a mapping/dictionary from letters to numbers (as chars)
translateChar :: Char -> Char
translateChar c = 
    case c of 
        'e' -> '3'
        'o' -> '0'
        'A' -> '4'
        't' -> '7'
        otherwise -> c