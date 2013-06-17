-- | Primitive combinators from which more complex patterns can be built.
module Data.Pattern.Prim where

import Prelude (Eq, String, Bool, Maybe (..), ($), flip)

import Control.Category ((.))
import Control.Isomorphism.Partial.Constructors (just, cons, nil)
import Control.Isomorphism.Partial.Prim
import Text.Syntax.Classes (ProductFunctor (..))

import Control.Isomorphism.Partial.Constructors.Extra (singleton, cdr)
import Control.Isomorphism.Partial.Prim.Extra (equals, fst, snd)
import Control.Isomorphism.Partial.Test (testIso)

import Data.Extract
import Data.Pattern.Env


-- | A term of type @a@ with holes of type @e@.
-- 
-- Each hole is a pattern-variable, matching a sub-term at that position.
-- 
-- >>> testPattern (1, 2) (var "x" <*> var "y") [("x",1), ("y",2)]
-- True
-- 
-- So-called \"pure\" patterns match exactly one value.
-- 
-- >>> testPattern 1 (pure 1) []
-- True
-- 
-- They are useful when only some of the subterms are pattern variables.
-- 
-- >>> testPattern (1, 2) (var "x" <*> pure 2) [("x",1)]
-- True
-- 
-- The combinator library is focused on matching ordered tuples, but you can
-- also use it on custom datatypes:
-- 
-- >>> testPattern (Just 1) (just <$> var "x") [("x",1)]
-- True
-- 
-- >>> testPattern [1] (cons <$> var "x" <*> pure []) [("x",1)]
-- True
-- 
-- The partial-isomorphisms library provides a convenient Template Haskell
-- function, @defineIsomorphisms@, which generates isomorphisms like @just@ and
-- @cons@ for your domain-specific algrebraic datatypes.
type Pattern e a = Extract (Env e) a

-- A pattern is represented by an isomorphism, whose two directions represent
-- substitution and pattern-matching.
-- 
-- >>> testIso [("x",1)] (runPattern $ just <$> var "x") (Just 1, [])
-- True
runPattern :: Pattern e a -> Iso (Env e) (a, Env e)
runPattern = runExtract

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
-- If the environment is incomplete, the substitution fails.
-- 
-- >>> subst (singleton <$> var "y") [("x",1)]
-- Nothing
-- 
-- Duplicate variables are allowed, and are consumed from left to right.
-- 
-- >>> subst (var "y" <*> var "x" <*> var "y") [("x","x1"), ("x","x2"), ("y","y1"), ("y","y2")]
-- Just ("y1",("x1","y2"))
subst :: Pattern e a -> Env e -> Maybe a
subst = extract

-- | Match a term against a pattern, extracting the sub-terms corresponding to
--   each variable.
-- 
-- >>> match (singleton <$> var "x") [1]
-- Just [("x",1)]
-- 
-- If the term doesn't match the pattern, the matching fails.
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
match p x = insert p x []


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
testPattern x p env = testExtract env p x []


-- | A pattern variable, the primitive pattern which matches anything.
-- 
-- >>> testPattern 1 (var "x") [("x",1)]
-- True
-- 
-- Surprisingly, that all we need! The extractor combinators take care of
-- everything else for us.
var :: String -> Pattern a a
var = Extract . lookup
