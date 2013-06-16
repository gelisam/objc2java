-- | Patterns unify substitution and pattern-matching.
-- 
-- Given a pattern with variables @x@ and @y@, we can either match against a
-- closed term to obtain values for @x@ and @y@, or in the other direction, we
-- can assign values to @x@ and @y@ in order to obtain a closed term. Indeed,
-- substitution is the inverse of pattern-matching!
module Data.Pattern
  ( Env
  , module Data.Pattern.Prim
  ) where

import Data.Pattern.Env
import Data.Pattern.Prim
