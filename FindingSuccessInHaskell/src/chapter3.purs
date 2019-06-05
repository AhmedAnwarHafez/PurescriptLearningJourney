module Ch3 where

import Data.Array (length, uncons)
import Data.Char.Unicode (isAlphaNum, isSpace)
import Data.Foldable (all)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Maybe (Maybe(..))
import Prelude

-- The function returns the password wrapped in 
-- a Maybe if only its length is not greater than 
-- 20 characters
checkPasswordLength :: String -> Maybe String
checkPasswordLength s =
    case (length $ toCharArray s) > 20 of
        true -> Nothing
        false -> Just s

-- check input string for non-special characters
-- and only allow for alpha numeric characters
requireAlphaNum :: String -> Maybe String
requireAlphaNum w =
    case (all isAlphaNum $ toCharArray w) of
        true -> Just w
        false -> Nothing

cleanWhitespace :: String -> Maybe String
cleanWhitespace "" = Nothing
cleanWhitespace s =
    case uncons $ toCharArray s of
        Just { head: x, tail: xs } ->  
            case isSpace x of
                true -> cleanWhitespace $ fromCharArray xs
                false -> Just $ fromCharArray $ [x] <> xs
        Nothing -> Nothing