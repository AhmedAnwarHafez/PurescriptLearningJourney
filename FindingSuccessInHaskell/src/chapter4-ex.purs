module Ch4e where
  
import Data.Maybe (Maybe(..))
import Prelude

-- Exercise 11:
-- If we pretend that the Maybe type has no instance of the Bind typeclass then
-- the following implementation will be it
bindMaybe :: forall a b. Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _ = Nothing
bindMaybe (Just a) f = f a

data StringOrValue a =
      Str String
    | Val a

-- Excercise 12
bindStringOrValue :: forall a b. 
      StringOrValue a
   -> (a -> StringOrValue b)  
   -> StringOrValue b
bindStringOrValue (Str s) _ = Str s
bindStringOrValue (Val a) f = f a