module Ch2 where

import Data.Array (sort)
import Data.Char.Unicode (isAlpha)
import Data.Foldable (all)
import Data.Maybe (Maybe(..))
import Data.String.Common as S
-- import toCharArray function to convert Strings into Array of Chars
-- Unlike Haskell, Purescript, by design, does not treat String as Array Char
-- So we first need to explicilty convert String to Array Char
import Data.String.CodeUnits (toCharArray) 
import Prelude

-- Takes two strings and check if both are anagram
-- "Silent" and "Listen" are anagrams
isAnagram :: String -> String -> Boolean
isAnagram word1 word2 = (sort $ toCharArray word1) == (sort $ toCharArray word2)

-- Check the word is not empty and does not contain numbers
isWord :: String -> Maybe String
isWord word = 
    case (S.null word) of
        true -> Nothing
        false -> 
            case (all isAlpha $ toCharArray word) of
                false -> Nothing
                true -> Just word

-- Take words and check if both are valid words before checking anagram
checkAnagram :: String -> String -> String
checkAnagram word1 word2 =
    case isWord word1 of
        Nothing -> "The first word is invalid"
        Just word1' -> 
            case (isWord word2) of
                Nothing -> "The second word is invalid"
                Just word2' -> 
                    case (isAnagram word1' word2') of
                        true -> "It is Anagram"
                        false -> "Nope"