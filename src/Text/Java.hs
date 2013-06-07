{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction, RelaxedPolyRec #-}
-- | An incomplete description of the Java syntax.
module Text.Java where

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


data Expr = Var String
          | Field { target :: Expr
                 , field_name :: String
                 }
     deriving (Show, Eq)

$(defineIsomorphisms ''Expr)


letter, digit :: Syntax delta => delta Char
letter  =  subset isLetter <$> token
digit   =  subset isDigit <$> token

identifier = cons <$> letter <*> many (letter <|> digit)

parens = between (text "(") (text ")")
brackets = between (text "[") (text "]")

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

unit' :: Iso ((), a) a
unit' = inverse (commute . unit)

drop_op :: Iso (a, ((), b)) (a, b)
drop_op = (id *** unit')


-- | The syntax of Java expressions.
-- 
-- Examples:
-- 
-- >>> parse expr "hello"
-- [Var "hello"]
-- 
-- >>> print expr (Var "hello")
-- Just "hello"
-- 
-- >>> parse expr "Hello.alloc.init"
-- [Field {target = Field {target = Var "Hello", field_name = "alloc"}, field_name = "init"}]
-- 
-- >>> print expr (Field (Field (Var "Hello") "alloc") "init")
-- Just "Hello.alloc.init"
expr = chainl1' (var <$> identifier) spacedDot identifier (field . drop_op)
