module Ch1 where

import Prelude 
import Data.String.Common as S
import Data.Tuple

-- Type 'a' here is polymorphic and is constrained by Ord
f :: forall a. Ord a => a -> a -> a
f x y = if x > y then x else y

-- Same function definition but the type signature is defined in terms of concrete type Int
f' :: Int -> Int -> Int
f' x y = if x > y then x else y

-- An example on how to control flow by using Case Expressions
-- case expressions can be used to test for booleans and other data types
f'' :: Int -> Int -> Int
f'' x y = 
    case (x > y) of
        true -> x + 10
        false -> y

-- Get the absolute value for any integer number using if-conditions
absVal :: Int -> Int
absVal x = 
    if x < 0
        then (negate x) -- <-- this function flips the sign
        else x

-- Another implementation to the function but using case expressions
absVal' :: Int -> Int
absVal' x = case (x > 0) of
    true -> x
    otherwise -> negate x
    -- or
    -- false -> negate x

validateUserNamePassword :: String -> String -> String
validateUserNamePassword u p = 
    case Tuple (S.null u) (S.null p) of
        Tuple false false -> "OK"
        Tuple true false -> "Empty username"
        Tuple false true -> "Empty password"
        Tuple true true -> "Empty username password"