module Control.Isomorphism.Partial.Extra where

import Prelude (Show (..), Read (..), Eq (..), String, Integer,
                map, (++), (&&), Maybe (..), Bool (..), ($), not, elem, 
                notElem, reads, Char, undefined)

import Control.Category (id, (.))

import Control.Monad (mplus)

import Data.Char (isLetter, isDigit)
import Data.Either

import qualified Text.ParserCombinators.Parsec as Parsec

import Control.Isomorphism.Partial
import Control.Isomorphism.Partial.TH
import Control.Isomorphism.Partial.Unsafe (Iso (Iso))
import Text.Syntax
import Text.Syntax.Parser.Naive
import Text.Syntax.Printer.Naive


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
