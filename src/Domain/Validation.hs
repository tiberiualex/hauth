module Domain.Validation where

import ClassyPrelude
import Text.Regex.PCRE.Heavy

-- for functions that receive any input and return a Maybe of any error messages
-- and Nothing if the input is valid
type Validation e a = a -> Maybe e

validate :: (a -> b) -> [Validation e a] -> a -> Either [e] b
validate constructor validations val =
  case concatMap (\f -> maybeToList $ f val) validations of
    []   -> Right $ constructor val
    errs -> Left errs

{- 
  concatMap - maps over and then concatenates = concat (map f xs) (not actual implementation)
  we apply all the validations to the value and then concatenate the error messages (if any)
  if no errors, apply the constructor function
-}

-- Generic function to check for the range for values of any Ord instance
rangeBetween :: (Ord a) => a -> a -> e -> Validation e a
rangeBetween minRange maxRange msg val =
  if val >= minRange && val <= maxRange then Nothing else Just msg

-- Typeclass I'm not familiar with https://stackoverflow.com/questions/39634504/is-there-anything-we-lose-with-monofoldable
-- https://hackage.haskell.org/package/mono-traversable-1.0.15.1/docs/Data-MonoTraversable.html#t:MonoFoldable
-- the MonoFoldable constraight here because the constraint of the length function from ClassyPrelude
lengthBetween :: (MonoFoldable a) => Int -> Int -> e -> Validation e a
lengthBetween minLen maxLen msg val =
  rangeBetween minLen maxLen msg (length val)

regexMatches :: Regex -> e -> Validation e Text
regexMatches regex msg val =
  if val =~ regex then Nothing else Just msg
