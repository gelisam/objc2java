{-# OPTIONS -XRankNTypes #-}
module Text.Syntax.Test where

import Prelude (Maybe (..), String)

import Data.Maybe (isJust)
import Control.Monad (guard)
import Text.Syntax (Syntax)
import Text.Syntax.Parser.Naive (parse)
import Text.Syntax.Printer.Naive (print)


testSyntax :: (forall s. Syntax s => s a) -> String -> Maybe String
testSyntax s input = do
  [ast] <- Just (parse s input)
  print s ast
