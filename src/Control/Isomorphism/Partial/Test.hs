module Control.Isomorphism.Partial.Test where

import Prelude (Eq (..), (&&), Maybe (..), Bool)

import Control.Isomorphism.Partial.Constructors (cons)
import Control.Isomorphism.Partial (Iso, apply, unapply)


-- | Bidirectional correctness: two tests for the price of one.
-- 
-- Isomorphisms are supposed to work in both direction, so you should test both:
-- 
-- >>> apply cons ('a',"")
-- Just "a"
-- 
-- >>> unapply cons "a"
-- Just ('a',"")
-- 
-- With testIso, you still test both directions, but you don't have to repeat yourself:
-- 
-- >>> testIso ('a',"") cons "a"
-- True
testIso :: (Eq a, Eq b) => a -> Iso a b -> b -> Bool
testIso x iso y =   apply iso x == Just y
               && unapply iso y == Just x
