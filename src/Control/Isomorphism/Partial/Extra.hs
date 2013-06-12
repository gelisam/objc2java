-- | A few useful tools when constructing isomorphisms.
module Control.Isomorphism.Partial.Extra where

import Prelude ()

import Control.Category (id, (.))
import Control.Isomorphism.Partial (Iso, (***), (|||),
                                    unapply, inverse, commute, associate,
                                    ignore, unit, nil, cons, listCases)
import Data.Either (Either)

import Control.Isomorphism.Partial.Test (testIso)

-- $setup
-- >>> let a2b = (inverse (ignore 'b')) . ignore 'a'


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

-- | Introduces nil in the same way as () is introduced, that is, in parallel to existing data.
-- 
-- The value [] is a lot like the value (), and the nil isomorphism recognizes this:
-- 
-- >>> testIso () nil []
-- True
-- 
-- Despite the similarity, the partial-isomorphisms library only allows () to be appended for free:
-- 
-- >>> testIso 'a' unit ('a', ())
-- True
-- 
-- The isomorphism append_nil fixes this minor oversight.
-- 
-- >>> testIso 'a' append_nil ('a', [])
-- True
append_nil :: Iso a (a, [b])
append_nil = snd nil . unit
  
-- | Invertible single-element list.
-- 
-- >>> testIso 'a' singleton "a"
-- True
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
