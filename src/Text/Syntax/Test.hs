{-# OPTIONS -XRankNTypes #-}
module Text.Syntax.Test where

import Prelude (Maybe (..), String)

import Data.Maybe (isJust)
import Control.Monad (guard)
import Text.Syntax
import Text.Syntax.Parser.Naive (parse)
import Text.Syntax.Printer.Naive (print)


-- | Correctness through whitespace-normalization: if an invertible syntax can
--   correctly remove all the spurious space, this is a good indication that
--   the parsing and printing phases were both successful.
-- 
-- >>> testSyntax (text "A" <* optSpace <*> text "B") "AB"
-- Just "A B"
-- 
-- >>> testSyntax (text "A" <* skipSpace <*> text "B") "A B"
-- Just "AB"
-- 
-- >>> testSyntax (text "a" <* skipSpace <*> text "b") "A B"
-- Nothing
testSyntax :: (forall s. Syntax s => s a) -> String -> Maybe String
testSyntax s input = do
  [ast] <- Just (parse s input)
  print s ast
