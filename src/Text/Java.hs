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

import Text.Common


data Expr = Var String
          | Field { target :: Expr
                 , field_name :: String
                 }
     deriving (Show, Eq)

$(defineIsomorphisms ''Expr)


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
