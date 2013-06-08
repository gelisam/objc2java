-- | Syntactic constructs shared by multiple languages.
module Text.Common where

import Prelude ((/=), String, Char, ($))

import Control.Category ((.))
import Control.Isomorphism.Partial
import Data.Char (isLetter, isDigit)
import Text.Syntax
import Text.Syntax.Parser.Naive (parse)

import Control.Isomorphism.Partial.Extra
import Text.Syntax.Test (testSyntax)


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
-- Escape sequences are preserved, but not interpreted.
-- 
-- >>> parse quoted_string "\"hello,\\n\n\\\"world\\\"\""
-- ["hello,\\n\n\\\"world\\\""]
quoted_string :: Syntax s => s String
quoted_string = text "\"" *> chars where
  chars = nil <$> text "\""
      <|> cons <$> non_escaped
               <*> chars
      <|> cons2 <$> escape
                <*> token
                <*> chars
  non_escaped = subset (/= '\\') <$> token
  escape = element '\\' <$> text "\\"
  cons2 :: Iso (a, (a, [a])) [a]
  cons2 = cons . snd cons

-- |
-- >>> testSyntax (parens $ text "A") "( A )"
-- Just "(A)"
parens, brackets :: Syntax s => s a -> s a
parens   = between (text "(" <* skipSpace) (skipSpace *> text ")")
brackets = between (text "[" <* skipSpace) (skipSpace *> text "]")

-- |
-- >>> testSyntax spacedDot " . "
-- Just "."
spacedDot :: Syntax s => s ()
spacedDot = between skipSpace skipSpace dot

-- |
-- >>> testSyntax spacedComma " , "
-- Just ", "
spacedComma :: Syntax s => s ()
spacedComma = skipSpace *> comma <* optSpace
