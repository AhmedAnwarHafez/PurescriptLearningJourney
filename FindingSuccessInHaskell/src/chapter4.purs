module Ch4 where
  
import Ch3  
import Data.Array (length, uncons)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Maybe (Maybe(..))
import Prelude

-- (>>=) :: forall a b m. Bind m => m a -> (a -> m b) -> m b

-- This is my attempt to improve password validation using the bind operator before
-- reading further in the book 
validatePasswordWithBind :: String -> Maybe String
validatePasswordWithBind password = do
    (cleanWhitespace password) >>= requireAlphaNum 

-- Another implementation but this time using the do-block
-- the value to the left of the arrow is fed to the next line
-- then finally we return the final value using pure (lift it inside the current monad)
validatePasswordWithDoNotation :: String -> Maybe String
validatePasswordWithDoNotation password = do
    p <- cleanWhitespace password
    p' <- requireAlphaNum p
    pure p'

validatePassword' :: String -> Maybe String
validatePassword' password = do
    (cleanWhitespace password) 
        >>= requireAlphaNum
        >>= checkPasswordLength

