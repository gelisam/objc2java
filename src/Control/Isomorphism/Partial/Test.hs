module Control.Isomorphism.Partial.Test where

import Prelude (Eq (..), (&&), Maybe (..), Bool)

import Control.Isomorphism.Partial (Iso, apply, unapply)


testIso :: (Eq a, Eq b) => a -> Iso a b -> b -> Bool
testIso x iso y =   apply iso x == Just y
               && unapply iso y == Just x
