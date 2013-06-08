module Text.Syntax.Extra where

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

import Control.Isomorphism.Partial.Extra


-- | A heterogeneous version of chainl1.
-- 
-- Examples:
-- 
-- >>> parse (chainl1 (text "A") (text ",") (ignore ((), ((), ())))) "A,A,A"
-- [()]
-- >>> parse (chainl1 (text "A") (text ",") (ignore ((), ((), ())))) "A,B,B"
-- []
-- >>> parse (chainl1' (text "A") (text ",") (text "B") (ignore ((), ((), ())))) "A,A,A"
-- []
-- >>> parse (chainl1' (text "A") (text ",") (text "B") (ignore ((), ((), ())))) "A,B,B"
-- [()]
chainl1' :: Syntax s => s a -> s b -> s c -> Iso (a, (b, c)) a -> s a
chainl1' arg0 op arg f = foldl f <$> arg0 <*> many (op <*> arg)

-- | A heterogeneous version of sepBy.
-- 
-- Examples:
-- 
-- >>> parse (sepBy (text "A") (text ",")) "A,A,A"
-- [[(),(),()]]
-- >>> parse (sepBy (text "A") (text ",")) "A,B,B"
-- []
-- >>> parse (sepBy' (text "A") (text ",") (text "B") (ignore ((), ()))) "A,A,A"
-- []
-- >>> parse (sepBy' (text "A") (text ",") (text "B") (ignore ((), ()))) "A,B,B"
-- [()]
sepBy' :: Syntax s => s a -> s () -> s b -> Iso (a, b) a -> s a
sepBy' arg0 op arg f = chainl1' arg0 op arg (f . drop_op)
                       where
  drop_left :: Iso ((), a) a
  drop_left = inverse (commute . unit)
  
  drop_op :: Iso (a, ((), b)) (a, b)
  drop_op = snd drop_left

-- | A non-empty version of sepBy.
-- 
-- Examples:
-- 
-- >>> parse (sepBy (text "A") (text ",")) "A"
-- [[()]]
-- 
-- >>> parse (sepBy (text "A") (text ",")) ""
-- [[]]
-- 
-- >>> parse (sepBy1 (text "A") (text ",")) "A"
-- [[()]]
-- 
-- >>> parse (sepBy1 (text "A") (text ",")) ""
-- []
sepBy1 :: Syntax delta => delta alpha -> delta () -> delta [alpha]
sepBy1 arg op = cons <$> arg <*> many (op *> arg) 
