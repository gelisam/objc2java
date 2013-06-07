{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction, RelaxedPolyRec #-}
-- | Syntactic constructs shared by multiple languages.
module Text.Common where

import Prelude (Show (..), Read (..), Eq (..), String, Integer,
                map, (++), Maybe (..), ($), fst, not, elem, 
                notElem, reads, Char)

import Control.Category (id, (.))

import Control.Monad (mplus)

import Data.Char (isLetter, isDigit)

import qualified Text.ParserCombinators.Parsec as Parsec

import Control.Isomorphism.Partial
import Control.Isomorphism.Partial.TH
import Control.Isomorphism.Partial.Unsafe (Iso (Iso))
import Text.Syntax
import Text.Syntax.Parser.Naive
import Text.Syntax.Printer.Naive



letter, digit :: Syntax s => s Char
letter  =  subset isLetter <$> token
digit   =  subset isDigit <$> token

identifier :: Syntax s => s String
identifier = cons <$> letter <*> many (letter <|> digit)

-- | A string literal.
-- 
-- Examples:
-- 
-- >>> parse quoted_string "hello"
-- []
-- 
-- >>> parse quoted_string "\"hello\""
-- ["hello"]
-- 
-- >>> parse quoted_string "\"hello"
-- []
-- 
-- >>> parse quoted_string "\"hello, \\\"world\\\"\""
-- ["hello, \"world\""]
quoted_string :: Syntax s => s String
quoted_string = between (text "\"") (text "\"") (many char) where
  char = non_quoted <|> quoted
  non_quoted = subset (/= '\\') <$> token
  quoted = text "\\" *> token

parens, brackets :: Syntax s => s a -> s a
parens = between (text "(") (text ")")
brackets = between (text "[") (text "]")

spacedDot :: Syntax s => s ()
spacedDot = between skipSpace skipSpace (text ".")


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
chainl1' arg0 op arg f 
  = foldl f <$> arg0 <*> many (op <*> arg)

drop_left :: Iso ((), a) a
drop_left = inverse (commute . unit)

drop_op :: Iso (a, ((), b)) (a, b)
drop_op = (id *** drop_left)
