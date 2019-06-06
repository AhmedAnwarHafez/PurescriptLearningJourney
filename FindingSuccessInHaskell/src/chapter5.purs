module Ch5 where
  
import Data.Array (length, uncons)
import Data.Char.Unicode (isAlphaNum, isSpace)
import Data.Foldable (all)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Prelude

-- validatePassword :: String -> String
-- validatePassword password = 
--     case (cleanWhitespace password) of
--         Nothing -> "Password cannot be empty"
--         Just password2 ->
--             case (requireAlphaNum password2) of
--                 Nothing -> "Password cannot contain white spaces or specical characters"
--                 Just password3 ->
--                     case checkPasswordLength password3 of
--                         Nothing -> "Password length should be between 10 and 20 characters"
--                         Just password4 -> password4

bindEither :: forall a b. Either b a -> (a -> Either b a) -> Either b a
bindEither (Left b) _ = Left b
bindEither (Right a) f = f a


checkPasswordLength :: String -> Either String String
checkPasswordLength password =
     case (length $ toCharArray password) > 20 of
        true -> Left "Password cannot be longer than 20 characters"
        false -> Right password


requireAlphaNum :: String -> Either String String
requireAlphaNum w =
    case (all isAlphaNum $ toCharArray w) of
        true -> Right w
        false -> Left "Password cannot contain white spaces or specical characters"

cleanWhitespace :: String -> Either String String
cleanWhitespace "" = Left "Password cannot be empty"
cleanWhitespace s =
    case uncons $ toCharArray s of
        Just { head: x, tail: xs } ->  
            case isSpace x of
                true -> cleanWhitespace $ fromCharArray xs
                false -> Right $ fromCharArray $ [x] <> xs
        Nothing -> Left "Password cannot be empty"

validatePassword :: String -> Either String String
validatePassword password =
    cleanWhitespace password
        >>= requireAlphaNum
        >>= checkPasswordLength