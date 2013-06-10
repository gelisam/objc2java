{-# LANGUAGE TemplateHaskell #-}
-- | An incomplete description of the ObjectiveC syntax.
module Text.ObjC where

import Prelude (Show, Eq, String, ($), flip)

import Control.Category ((.))
import Control.Isomorphism.Partial (Iso, (<$>))
import Control.Isomorphism.Partial.TH (defineIsomorphisms)
import Text.Syntax

import Control.Isomorphism.Partial.Extra
import Text.Common
import Text.Syntax.Extra (sepBy1)
import Text.Syntax.Test (testSyntax)


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
-- >>> testSyntax expr "[ [ Hello alloc ]init ]"
-- Just "[[Hello alloc] init]"
-- 
-- >>> testSyntax expr "NSLog ( @\"Hello, World!\" )"
-- Just "NSLog(@\"Hello, World!\")"
-- 
-- >>> testSyntax expr "NSLog ( @\"The current date and time is: %@\", [ NSDate  date ] )"
-- Just "NSLog(@\"The current date and time is: %@\", [NSDate date])"
-- 
-- >>> testSyntax expr "[ myData  writeToFile:@\"/tmp/log.txt\" atomically:  NO ]"
-- Just "[myData writeToFile: @\"/tmp/log.txt\" atomically: NO]"
expr :: Syntax s => s Expr
expr = stringLit <$> text "@" *> quoted_string
   <|> brackets (zeroary_methodCall <$> expr <*> optSpace *> identifier)
   <|> brackets (methodCall . snd unzip <$> expr <*> optSpace *> named_args)
   <|> functionCall <$> identifier <* skipSpace <*> positional_args
   <|> var <$> identifier
       where
  positional_args = parens (sepBy expr spacedComma)
  
  zeroary_methodCall :: Iso (Expr, String) Expr
  zeroary_methodCall = methodCall . snd append_nil . snd singleton
  
  named_args :: Syntax s => s [(String, Expr)]
  named_args = flip sepBy1 optSpace $ identifier <* text ":" <* optSpace <*> expr
