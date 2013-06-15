-- | A few useful tools when constructing isomorphisms.
module Control.Isomorphism.Partial.Prim.Extra where

import Prelude (Eq (..), Int, Maybe (..), Num (..), const)

import Control.Category (id, (.))
import Control.Isomorphism.Partial
import Control.Isomorphism.Partial.Unsafe (Iso (..))
import Data.Either (Either)
import Text.Syntax.Classes (Alternative (..))

import Control.Isomorphism.Partial.Test (testIso)


-- $setup
-- >>> let a2b = (inverse (equals 'b')) . equals 'a'

-- | Expects a particular value.
-- 
-- >>> testIso 'a' (equals 'a') ()
-- True
-- 
-- Fails otherwise. Useful when you have a backup isomorphism to fall back to
-- (see below).
-- 
-- >>> apply (equals 'a') 'b'
-- Nothing
equals :: Eq a => a -> Iso a ()
equals x = ignore x . subset (==x)

-- | The isomorphisms are partial, meaning they can fail.
--   When they do, you can fall back to a second isomorphism instead.
-- 
-- >>> testIso 'a' (empty <|> a2b) 'b'
-- True
-- 
-- Since the first isomorphism is preferred in both directions, it is possible
-- to construct situations where the two directions are not inverses of each
-- other.
-- 
-- >>> apply (equals 'a' <|> equals 'b') 'b'
-- Just ()
-- 
-- >>> unapply (equals 'a' <|> equals 'b') ()
-- Just 'a'
-- 
-- This possibility is actually a feature of partial isomorphisms:
-- 
-- >>> apply (ignore 'a') 'b'
-- Just ()
-- 
-- >>> unapply (ignore 'a') ()
-- Just 'a'
-- 
-- Of course, if the two isomorphism don't overlap, that "problem" will never
-- happen.
-- 
-- >>> testIso 'b' (a2b <|> inverse a2b) 'a'
-- True
instance Alternative (Iso a) where
  empty = Iso (const Nothing)
              (const Nothing)
  x <|> y = (x ||| y) . (x1 ||| y2) . inverse (x' ||| y') where
    x' = inverse x
    y' = inverse y
    x1 = left . x'
    y2 = right . y'


-- | Apply an isomorphism to the left part of a pair.
-- 
-- >>> testIso ('a', 'a') (fst a2b) ('b', 'a')
-- True
fst :: Iso a a' -> Iso (a, b) (a', b)
fst = (*** id)

-- | Apply an isomorphism to the right part of a pair.
-- 
-- >>> testIso ('a', 'a') (snd a2b) ('a', 'b')
-- True
snd :: Iso b b' -> Iso (a, b) (a, b')
snd = (id ***)
