module Control.Isomorphism.Partial.Extra where

import Prelude (Eq (..), (&&), Maybe (..), Bool)

import Control.Category (id, (.))
import Control.Isomorphism.Partial (Iso, (***), (|||),
                                    apply, unapply, inverse, commute, associate,
                                    unit, nil, cons, listCases)
import Data.Either (Either)


testIso :: (Eq a, Eq b) => a -> Iso a b -> b -> Bool
testIso x iso y =   apply iso x == Just y
               && unapply iso y == Just x

fst :: Iso a a' -> Iso (a, b) (a', b)
fst = (*** id)

snd :: Iso b b' -> Iso (a, b) (a, b')
snd = (id ***)

append_nil :: Iso a (a, [b])
append_nil = snd nil . unit
  
singleton :: Iso a [a]
singleton = cons . append_nil

-- | Invertible zip, fails if the lists have different lengths.
-- 
-- Examples:
-- 
-- >>> testIso [(1, '1'), (2, '2'), (3, '3')] unzip ([1,2,3], "123")
-- True
-- 
-- >>> unapply unzip ([1,2,3], "123")
-- Just [(1,'1'),(2,'2'),(3,'3')]
-- 
-- >>> unapply unzip ([1,2], "123")
-- Nothing
unzip :: Iso [(a, b)] ([a], [b])
unzip = split . inverse listCases where
  split :: Iso (Either () ((a, b), [(a, b)])) ([a], [b])
  split = nils
      ||| (conses . snd unzip)
  
  nils :: Iso () ([a], [b])
  nils = (nil *** nil) . unit
  
  conses :: Iso ((a, b), ([a], [b])) ([a], [b])
  conses = (cons *** cons) . redistribute
  
  redistribute :: Iso ((a, b), (a', b'))
                      ((a, a'), (b, b'))
  redistribute = associate
               . snd ( inverse associate
                     . fst commute
                     . associate)
               . inverse associate
