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
          | MethodCall { target :: Expr
                       , method_name :: String
                       }
          | FunctionCall { function_name :: String
                         , arg :: Expr
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
-- [MethodCall {target = MethodCall {target = Var "Hello", method_name = "alloc"}, method_name = "init"}]
-- 
-- >>> print expr (MethodCall (MethodCall (Var "Hello") "alloc") "init")
-- Just "[[Hello alloc] init]"
-- 
-- >>> parse expr "NSLog(msg)"
-- [FunctionCall {function_name = "NSLog", arg = Var "msg"}]
-- 
-- >>> print expr (FunctionCall "NSLog" (Var "msg"))
-- Just "NSLog(msg)"
expr :: Syntax s => s Expr
expr = var <$> identifier
   <|> brackets (methodCall <$> expr <*> optSpace *> identifier)
   <|> functionCall <$> identifier <*> parens (expr)
