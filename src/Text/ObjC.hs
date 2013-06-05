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


data Expr = Var String
          | Call { method_name :: String
                 , args :: [Expr]
                 }
     deriving (Show, Eq)

$(defineIsomorphisms ''Expr)


letter, digit :: Syntax delta => delta Char
letter  =  subset isLetter <$> token
digit   =  subset isDigit <$> token

identifier = cons <$> letter <*> many (letter <|> digit)

parens = between (text "(") (text ")")
brackets = between (text "[") (text "]")


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
-- -- >>> parse expr "[[Hello alloc] init]"
-- -- [Call "init" [Call "alloc" [Var "Hello"]]]
--
-- -- >>> print expr (Call "init" [Call "alloc" [Var "Hello"]])
-- -- Just "[[Hello alloc] init]"
expr = var <$> identifier
