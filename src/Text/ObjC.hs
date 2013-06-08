{-# LANGUAGE TemplateHaskell #-}
-- | An incomplete description of the ObjectiveC syntax.
module Text.ObjC where

import Prelude (Show, Eq, String, ($), flip)

import Control.Category ((.))
import Control.Isomorphism.Partial (Iso, (<$>))
import Control.Isomorphism.Partial.TH (defineIsomorphisms)
import Text.Syntax
import Text.Syntax.Parser.Naive (parse)
import Text.Syntax.Printer.Naive (print)

import Control.Isomorphism.Partial.Extra
import Text.Common
import Text.Syntax.Extra (sepBy1)


data Expr = Var String
          | StringLit String
          | MethodCall { target :: Expr
                       , method_name :: [String]
                       , args :: [Expr]
                       }
          | FunctionCall { function_name :: String
                         , args :: [Expr]
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
-- [MethodCall {target = MethodCall {target = Var "Hello", method_name = ["alloc"], args = []}, method_name = ["init"], args = []}]
-- 
-- >>> print expr (MethodCall (MethodCall (Var "Hello") ["alloc"] []) ["init"] [])
-- Just "[[Hello alloc] init]"
-- 
-- >>> parse expr "NSLog(@\"Hello, World!\")"
-- [FunctionCall {function_name = "NSLog", args = [StringLit "Hello, World!"]}]
-- 
-- >>> print expr (FunctionCall "NSLog" [Var "msg"])
-- Just "NSLog(msg)"
-- 
-- >>> parse expr "NSLog ( @\"The current date and time is: %@\", [NSDate date] )"
-- [FunctionCall {function_name = "NSLog", args = [StringLit "The current date and time is: %@",MethodCall {target = Var "NSDate", method_name = ["date"], args = []}]}]
-- 
-- >>> print expr (FunctionCall "NSLog" [StringLit "The current date and time is: %@", MethodCall (Var "NSDate") ["date"] []])
-- Just "NSLog(@\"The current date and time is: %@\", [NSDate date])"
-- 
-- >>> parse expr "[myData writeToFile:@\"/tmp/log.txt\" atomically: NO]"
-- [MethodCall {target = Var "myData", method_name = ["writeToFile","atomically"], args = [StringLit "/tmp/log.txt",Var "NO"]}]
expr :: Syntax s => s Expr
expr = var <$> identifier
   <|> stringLit <$> text "@" *> quoted_string
   <|> brackets (zeroary_methodCall <$> expr <*> optSpace *> identifier)
   <|> brackets (methodCall . snd unzip <$> expr <*> optSpace *> named_args)
   <|> functionCall <$> identifier <* skipSpace <*> positional_args
       where
  positional_args = parens (sepBy expr spacedComma)
  
  zeroary_methodCall :: Iso (Expr, String) Expr
  zeroary_methodCall = methodCall . snd append_nil . snd singleton
  
  named_args :: Syntax s => s [(String, Expr)]
  named_args = flip sepBy1 skipSpace $ identifier <* text ":" <* skipSpace <*> expr
