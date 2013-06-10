{-# LANGUAGE TemplateHaskell #-}
-- | An incomplete description of the Java syntax.
module Text.Java where

import Prelude (Show, Eq, String)

import Control.Category ((.))
import Control.Isomorphism.Partial
import Control.Isomorphism.Partial.TH (defineIsomorphisms)
import Text.Syntax

import Text.Common
import Text.Syntax.Extra (sepBy')
import Text.Syntax.Test (testSyntax)


data Expr = Var String
          | StringLit String
          | Field { target :: Expr
                 , field_name :: String
                 }
          | ConstructorCall { class_name :: String
                            , args :: [Expr]
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
-- >>> testSyntax expr "Hello . alloc . init"
-- Just "Hello.alloc.init"
-- 
-- >>> testSyntax expr "Hello . alloc () . init( world )"
-- Just "Hello.alloc().init(world)"
-- 
-- >>> testSyntax expr "new Hello ( world )"
-- Just "new Hello(world)"
-- 
-- >>> testSyntax expr "println ( \"The current date and time is: %@\" , DateTime.now() )"
-- Just "println(\"The current date and time is: %@\", DateTime.now())"
expr :: Syntax s => s Expr
expr = sepBy' (var <$> identifier) spacedDot member (member_iso . distribute)
   <|> functionCall <$> identifier <* skipSpace <*> args
   <|> constructorCall <$> text "new " *> identifier <* skipSpace <*> args
   <|> stringLit <$> quoted_string
       where
  args = parens (sepBy expr spacedComma)
  member = left <$> identifier
       <|> right <$> identifier <* skipSpace <*> args
  member_iso = field
           ||| methodCall
