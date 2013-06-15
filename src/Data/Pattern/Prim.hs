-- | Primitive combinators from which more complex patterns can be built.
module Data.Pattern.Prim where

import Prelude (Eq, String, Bool, Maybe (..), return)

import Control.Category ((.))
import Control.Isomorphism.Partial.Constructors (just, cons, nil)
import Control.Isomorphism.Partial.Prim
import Text.Syntax.Classes (ProductFunctor (..))

import Control.Isomorphism.Partial.Constructors.Extra (singleton, cdr)
import Control.Isomorphism.Partial.Prim.Extra (equals, fst, snd)
import Control.Isomorphism.Partial.Test (testIso)

import Data.Pattern.Env


-- | A term of type @a@ with holes of type @e@.
-- 
-- A pattern is represented by an isomorphism, whose two directions represent
-- substitution and pattern-matching.
newtype Pattern e a = Pattern { runPattern :: Iso (Env e) (a, Env e) }

-- | Plug in all the holes to construct a closed term.
-- 
-- >>> subst (singleton <$> var "x") [("x",1)]
-- Just [1]
-- 
-- If the environment contains unused values, they are ignored.
-- 
-- >>> subst (singleton <$> var "x") [("x",1), ("y",2)]
-- Just [1]
-- 
-- >>> subst (singleton <$> var "y") [("x",1), ("y",2)]
-- Just [2]
-- 
-- Of course, if the environment doesn't provide enough values, the computation
-- fails.
-- 
-- >>> subst (singleton <$> var "y") [("x",1)]
-- Nothing
-- 
-- Duplicate variables are allowed, and are consumed from left to right.
-- 
-- >>> subst (var "y" <*> var "x" <*> var "y") [("x","x1"), ("x","x2"), ("y","y1"), ("y","y2")]
-- Just ("y1",("x1","y2"))
subst :: Pattern e a -> Env e -> Maybe a
subst p env = do
  (x, _) <- apply (runPattern p) env
  return x

-- | Match a term against a pattern to extract the sub-terms which match the
--   variables.
-- 
-- >>> match (singleton <$> var "x") [1]
-- Just [("x",1)]
-- 
-- Of course, if the term doesn't match the pattern, then the pattern-matching
-- fails.
-- 
-- >>> match (singleton <$> var "x") [1, 2]
-- Nothing
-- 
-- Duplicate pattern variables are allowed, in which case duplicate entries are
-- added to the environment in the same order.
-- 
-- >>> match (var "y" <*> var "x" <*> var "y") (1, (2, 3))
-- Just [("y",1),("x",2),("y",3)]
match :: Pattern e a -> a -> Maybe (Env e)
match p x = unapply (runPattern p) (x, [])


-- | Bidirectional correctness: two tests for the price of one.
-- 
-- Patterns are supposed to work in both direction, so you should test
-- both:
-- 
-- >>> subst (singleton <$> var "x") [("x",1)]
-- Just [1]
-- 
-- >>> match (singleton <$> var "x") [1]
-- Just [("x",1)]
-- 
-- With @testPattern@, you still test both directions, but you don't have to
-- repeat yourself:
-- 
-- >>> testPattern [1] (singleton <$> var "x") [("x",1)]
-- True
testPattern :: (Eq e, Eq a) => a -> Pattern e a -> Env e -> Bool
testPattern x p env = testIso env (runPattern p) (x,[])


-- | A pattern variable, the primitive pattern which matches anything.
-- 
-- >>> testPattern 1 (var "x") [("x",1)]
-- True
var :: String -> Pattern a a
var = Pattern . lookup

-- | A pure value, the primitive pattern which only matches one value.
-- 
-- >>> testPattern 1 (pure 1) []
-- True
-- 
-- This is useful when only some of the subterms are pattern variables.
-- 
-- >>> testPattern (1, 2) (var "x" <*> pure 2) [("x",1)]
-- True
-- 
-- The nomenclature for the name @pure@ is taken from the invertible-syntax
-- library, which has a function of the same name with a similar meaning.
pure :: Eq a => a -> Pattern e a
pure = Pattern . check where
  check :: Eq a => a -> Iso (Env e) (a, Env e)
  check x = inverse (cdr . car_matches x)
  
  car_matches :: Eq a => a -> Iso ( a, Env e)
                                  ((), Env e)
  car_matches = fst . equals


-- | A pair of patterns, for matching tuples.
-- 
-- >>> testPattern (1, 2) (var "x" <*> var "y") [("x",1), ("y",2)]
-- True
instance ProductFunctor (Pattern e) where
  p <*> q = Pattern (associate . snd q' . p') where
    p' = runPattern p
    q' = runPattern q

-- | Use isomorphisms to match custom datatypes.
-- 
-- The combinator library is focused on matching ordered tuples, but you can
-- also match on a custom constructor by using an isomorphism to bridge the
-- gap between the constructor arguments and an ordered tuple containing those
-- same arguments. The partial-isomorphisms library provides a convenient
-- Template Haskell function, @defineIsomorphisms@, which generates those
-- isomorphisms for you.
-- 
-- >>> testPattern (Just 1) (just <$> var "x") [("x",1)]
-- True
-- 
-- >>> testPattern [1,2] (cons <$> var "x" <*> (cons <$> var "y" <*> pure [])) [("x",1),("y",2)]
-- True
instance IsoFunctor (Pattern e) where
  iso <$> p = Pattern (fst iso . runPattern p)
