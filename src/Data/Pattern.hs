-- | Simultaneously define pattern-matching and a substitution.
-- 
-- Given a pattern with variables X and Y, we can either match against a closed
-- term to obtain values for X and Y, or in the other direction, we can assign
-- values to X and Y in order to obtain a closed term. Indeed, substitution is
-- the inverse of pattern-matching!
module Data.Pattern (Env) where

import Data.Pattern.Env
