-- | A few useful list-manipulation isomorphisms.
module Control.Isomorphism.Partial.Constructors.Extra where

import Prelude (Eq (..), Int, Maybe (..), Num (..), const)

import Control.Category (id, (.))
import Control.Isomorphism.Partial
import Control.Isomorphism.Partial.Unsafe (Iso (..))
import Data.Either (Either)
import Text.Syntax.Classes (Alternative (..))

import Control.Isomorphism.Partial.Prim.Extra (fst, snd)
import Control.Isomorphism.Partial.Test (testIso)


-- | Introduces nil in the same way as @()@ is introduced, that is, in parallel
--   to existing data.
-- 
-- The value @[]@ is a lot like the value @()@, and the @nil@ isomorphism
-- recognizes this:
-- 
-- >>> testIso () nil []
-- True
-- 
-- Despite the similarity, the partial-isomorphisms library only allows @()@ to
-- be appended for free:
-- 
-- >>> testIso 'a' unit ('a', ())
-- True
-- 
-- The isomorphism @append_nil@ fixes this minor oversight.
-- 
-- >>> testIso 'a' append_nil ('a', [])
-- True
append_nil :: Iso a (a, [b])
append_nil = snd nil . unit
  
-- | Invertibly pack a single element into a list.
-- 
-- >>> testIso 'a' singleton "a"
-- True
singleton :: Iso a [a]
singleton = cons . append_nil

-- | Invertibly discard the left element; something which can only be done
--   invertibly if that element is @()@.
-- 
-- >>> testIso ((), 'a') cdr 'a'
-- True
cdr :: Iso ((), a) a
cdr = inverse (commute . unit)

-- | Swaps the first two elements of an ordered tuple.
-- 
-- >>> testIso (1, (2, "...")) swap (2, (1, "..."))
-- True
-- 
-- Swapping other elements than the first two is easy:
-- 
-- >>> testIso (0, (1, (2, "..."))) (snd swap) (0, (2, (1, "...")))
-- True
swap :: Iso (a, (b, r)) (b, (a, r))
swap = inverse associate
     . fst commute
     . associate
