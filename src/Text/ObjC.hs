{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction, RelaxedPolyRec #-}
-- | An incomplete description of the ObjectiveC syntax.
module Text.ObjC where

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
          | Call { target :: Expr
                 , method_name :: String
                 }
     deriving (Show, Eq)

$(defineIsomorphisms ''Expr)


-- | The syntax of ObjectiveC expressions.
-- 
-- Examples:
-- 
-- >>> parse expr "Hello"
-- [Var "Hello"]
-- 
-- >>> print expr (Var "Hello")
-- Just "Hello"
-- 
-- >>> parse expr "[[Hello alloc] init]"
-- [Call {target = Call {target = Var "Hello", method_name = "alloc"}, method_name = "init"}]
-- 
-- >>> print expr (Call (Call (Var "Hello") "alloc") "init")
-- Just "[[Hello alloc] init]"
expr = var <$> identifier
   <|> brackets (call <$> (expr <*> optSpace *> identifier))
