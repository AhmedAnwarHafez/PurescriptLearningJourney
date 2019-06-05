module Ch3e where

import Ch3
import Data.Array (length, uncons)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Maybe (Maybe(..))
import Prelude

-- The function returns the password wrapped in 
-- a Maybe if only its length is between 10 and 20 characters
checkPasswordLength' :: String -> Maybe String
checkPasswordLength' s =
    case (length $ toCharArray s) > 20 || (length $ toCharArray s) < 10 of
        true -> Nothing
        false -> Just s

validatePassword :: String -> Maybe String
validatePassword password =
    case cleanWhitespace password of
        Nothing -> Nothing
        Just cleaned ->
            case checkPasswordLength' cleaned of
                Nothing -> Nothing
                Just _ -> 
                    case requireAlphaNum cleaned of
                        Nothing -> Nothing
                        Just _ -> Just cleaned



