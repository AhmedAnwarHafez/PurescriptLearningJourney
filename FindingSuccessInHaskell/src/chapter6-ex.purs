module Ch6e where

import Ch6 (Error(..), Username(..), cleanWhitespace, requireAlphaNum, checkUsernameLength)
import Data.Array (length, uncons)
import Data.Char.Unicode (isAlphaNum, isSpace)
import Data.Foldable (all)
import Data.Either (Either(..))  
import Data.Maybe (Maybe(..))  
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Prelude

validateUsername :: Username -> Either Error Username
validateUsername (Username u) =
    cleanWhitespace u
        >>= requireAlphaNum
        >>= checkUsernameLength

checkLength :: Int -> String -> Either Error String
checkLength maxLength s =
    case (length $ toCharArray s) > maxLength of
        true -> Left $ Error $ "Cannot exceed " <> show maxLength
        false -> Right (s)

validateUsername' :: Username -> Either Error Username
validateUsername' (Username u) = do
    cleaned <- cleanWhitespace u
    alphaNum <- requireAlphaNum cleaned
    checkUsernameLength alphaNum