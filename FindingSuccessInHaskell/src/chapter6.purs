module Ch6 where

import Data.Array (length, uncons)
import Data.Char.Unicode (isAlphaNum, isSpace)
import Data.Foldable (all)
import Data.Either (Either(..))  
import Data.Maybe (Maybe(..))  
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Prelude

newtype Password = Password String
instance showPassword :: Show Password where
    show (Password s) = "(Password \"" <> s <> "\")"

newtype Error = Error String
instance showError :: Show Error where
    show (Error e) = "(Error \"" <> e <> "\")"

newtype Username = Username String
instance showUsername :: Show Username where
    show (Username u) = "(Username \"" <> u <> "\")"

checkPasswordLength :: String -> Either Error Password
checkPasswordLength password =
     case (length $ toCharArray password) > 20 of
        true -> Left (Error "Password cannot be longer than 20 characters")
        false -> Right (Password password)

checkUsernameLength :: String -> Either Error Username
checkUsernameLength u =
    case (length $ toCharArray u) > 15 of
        true -> Left (Error "Username cannot be longer than 15 character")
        false -> Right (Username u)

requireAlphaNum :: String -> Either Error String
requireAlphaNum w =
    case (all isAlphaNum $ toCharArray w) of
        true -> Right w
        false -> Left (Error "Password cannot contain white spaces or specical characters")

cleanWhitespace :: String -> Either Error String
cleanWhitespace "" = Left (Error "Password cannot be empty")
cleanWhitespace s =
    case uncons $ toCharArray s of
        Just { head: x, tail: xs } ->  
            case isSpace x of
                true -> cleanWhitespace $ fromCharArray xs
                false -> Right $ fromCharArray $ [x] <> xs
        Nothing -> Left (Error "Password cannot be empty")

validatePassword :: Password -> Either Error Password
validatePassword (Password p) =
    cleanWhitespace p
        >>= requireAlphaNum
        >>= checkPasswordLength

