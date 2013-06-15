-- | A few useful tools when constructing isomorphisms.
module Control.Isomorphism.Partial.Derived.Extra where

import Prelude (Eq (..), Int, Maybe (..), Num (..), const)

import Control.Category (id, (.))
import Control.Isomorphism.Partial
import Control.Isomorphism.Partial.Unsafe (Iso (..))
import Data.Either (Either)
import Text.Syntax.Classes (Alternative (..))

import Control.Isomorphism.Partial.Constructors.Extra (append_nil, swap)
import Control.Isomorphism.Partial.Prim.Extra (fst, snd)
import Control.Isomorphism.Partial.Test (testIso)


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
               . snd swap
               . inverse associate

-- | Invertible splitAt, where the inverse is of course concatenation.
-- 
-- >>> testIso "12345" (splitAt 3) ("123","45")
-- True
-- 
-- >>> testIso "12" (splitAt 3) ("12","")
-- True
splitAt :: Int -> Iso [a] ([a], [a])
splitAt 0 = commute . append_nil
splitAt n = (caseNil ||| caseCons) . inverse listCases where
  caseNil :: Iso () ([a], [a])
  caseNil = append_nil . nil
  
  caseCons :: Iso (a, [a]) ([a], [a])
  caseCons = fst cons . associate . snd (splitAt n')
  
  n' = n - 1
