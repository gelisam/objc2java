-- | Primitive combinators from which more complex extractors can be built.
module Data.Extract where

import Prelude (Eq, Bool (..), Maybe (..), ($), fmap, curry)
import qualified Prelude (fst, snd)

import Control.Category ((.))
import Control.Isomorphism.Partial.Constructors (just, cons)
import Control.Isomorphism.Partial.Prim
import Text.Syntax.Classes (ProductFunctor (..))

import Control.Isomorphism.Partial.Constructors.Extra (cdr, swap)
import Control.Isomorphism.Partial.Prim.Extra (equals)
import qualified Control.Isomorphism.Partial.Prim.Extra as Iso (fst, snd)
import Control.Isomorphism.Partial.Test (testIso)


-- $setup
-- >>> let extract_head = Extract $ inverse cons


-- | An extractor, which invertibly extracts a value of type @a@ out of an
--   environment of type @e@.
-- 
-- >>> testExtract [1,2,3] extract_head 1 [2,3]
-- True
newtype Extract e a = Extract { runExtract :: Iso e (a, e) }

-- | Extract a value out of the environment.
-- 
-- >>> extract extract_head [1,2,3]
-- Just 1
-- 
-- If possible.
-- 
-- >>> extract extract_head []
-- Nothing
extract :: Extract e a -> e -> Maybe a
extract e = fmap Prelude.fst . apply (runExtract e)

-- | Removes a value from the environment, returning the reduced environment.
-- 
-- >>> remove extract_head [1,2,3]
-- Just [2,3]
-- 
-- If possible.
-- 
-- >>> remove extract_head []
-- Nothing
remove :: Extract e a -> e -> Maybe e
remove e = fmap Prelude.snd . apply (runExtract e)

-- | Add an extra value to an existing environment. This uses the fact that the
--   extractor is invertible.
-- 
-- >>> insert extract_head 1 [2,3]
-- Just [1,2,3]
-- 
-- Inserting is much more likely to succeed than the other two operations, but
-- failure is still possible.
-- 
-- >>> insert (pure 1) 2 ()
-- Nothing
insert :: Extract e a -> a -> e -> Maybe e
insert = curry . unapply . runExtract

-- | Three tests for the price of one.
-- 
-- You need three at least three checks to test an extractor:
-- 
-- >>> extract extract_head [1,2,3]
-- Just 1
-- 
-- >>> remove extract_head [1,2,3]
-- Just [2,3]
-- 
-- >>> insert extract_head 1 [2,3]
-- Just [1,2,3]
-- 
-- With @testExtract@, you still perform three tests, but you don't have to
-- repeat yourself:
-- 
-- >>> testExtract [1,2,3] extract_head 1 [2,3]
-- True
testExtract :: (Eq e, Eq a) => e -> Extract e a -> a -> e -> Bool
testExtract env e x env' = testIso env (runExtract e) (x,env')

-- | A pure value, which doesn't need to be extracted from anything.
-- 
-- >>> testExtract () (pure "a") "a" ()
-- True
-- 
-- The environment is left completely untouched.
-- 
-- >>> testExtract [1,2,3] (pure "a") "a" [1,2,3]
-- True
-- 
-- The nomenclature for the name @pure@ is taken from the invertible-syntax
-- library, which has a function of the same name with a similar meaning.
pure :: Eq a => a -> Extract s a
pure = Extract . check where
  check :: Eq a => a -> Iso s (a, s)
  check x = inverse (cdr . car_matches x)
  
  car_matches :: Eq a => a -> Iso (a, s) ((), s)
  car_matches = Iso.fst . equals


-- | A pair of extractor, for extracting tuples.
-- 
-- >>> testExtract () (pure "a" <*> pure "b") ("a","b") ()
-- True
-- 
-- The two values are extracted from the same environment, one after the other.
-- 
-- >>> testExtract [1,2,3] (extract_head <*> extract_head) (1,2) [3]
-- True
instance ProductFunctor (Extract e) where
  l <*> r = Extract (associate . Iso.snd r' . l') where
    l' = runExtract l
    r' = runExtract r

-- | Use isomorphisms to extract custom datatypes.
-- 
-- The combinator library is focused on extracting ordered tuples, but you can
-- also extract custom types by using an isomorphism to bridge the gap between
-- a constructor and an ordered tuple containing the arguments expected by this
-- constructor. The partial-isomorphisms library provides a convenient Template
-- Haskell function, @defineIsomorphisms@, which generates those isomorphisms
-- for you.
-- 
-- >>> testExtract [1,2,3] (just <$> extract_head) (Just 1) [2,3]
-- True
-- 
-- >>> testExtract [1,2,3] (cons <$> extract_head <*> pure []) [1] [2,3]
-- True
instance IsoFunctor (Extract e) where
  iso <$> e = Extract $ Iso.fst iso . runExtract e
