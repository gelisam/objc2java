{-# LANGUAGE TemplateHaskell #-}
-- | An incomplete description of the Java syntax.
module Text.Java where

import Prelude (Show, Eq, String)

import Control.Category ((.))
import Control.Isomorphism.Partial
import Control.Isomorphism.Partial.TH (defineIsomorphisms)
import Text.Syntax
import Text.Syntax.Parser.Naive (parse)
import Text.Syntax.Printer.Naive (print)

import Text.Common
import Text.Syntax.Extra (sepBy')


data Expr = Var String
          | StringLit String
          | Field { target :: Expr
                 , field_name :: String
                 }
          | MethodCall { target :: Expr
                       , method_name :: String
                       , args :: [Expr]
                       }
          | FunctionCall { function_name :: String
                         , args :: [Expr]
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
-- 
-- >>> parse expr "Hello.alloc().init(world)"
-- [MethodCall {target = MethodCall {target = Var "Hello", method_name = "alloc", args = []}, method_name = "init", args = [Var "world"]}]
-- 
-- >>> print expr (MethodCall (MethodCall (Var "Hello") "alloc" []) "init" [Var "world"])
-- Just "Hello.alloc().init(world)"
-- 
-- >>> parse expr "println ( \"The current date and time is: %@\", DateTime.now() )"
-- [FunctionCall {function_name = "println", args = [StringLit "The current date and time is: %@",MethodCall {target = Var "DateTime", method_name = "now", args = []}]}]
-- 
-- >>> print expr (FunctionCall "println" [StringLit "The current date and time is: %@", MethodCall (Var "DateTime") "now" []])
-- Just "println(\"The current date and time is: %@\", DateTime.now())"
expr :: Syntax s => s Expr
expr = sepBy' (var <$> identifier) spacedDot member (member_iso . distribute)
   <|> functionCall <$> identifier <* skipSpace <*> args
   <|> stringLit <$> quoted_string
       where
  args = parens (sepBy expr spacedComma)
  member = left <$> identifier
       <|> right <$> identifier <* skipSpace <*> args
  member_iso = field
           ||| methodCall
