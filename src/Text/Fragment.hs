{-# LANGUAGE TemplateHaskell #-}
-- | Incomplete grammar? Leave the unrecognized parts alone.
module Text.Fragment where

import Prelude (Show, Eq, Char, ($), head)

import Control.Isomorphism.Partial ((<$>))
import Control.Isomorphism.Partial.TH (defineIsomorphisms)
import Text.Syntax
import Text.Syntax.Parser.Naive (parse)


data Fragment a = Recognized a
                | Unrecognized Char
     deriving (Show, Eq)

$(defineIsomorphisms ''Fragment)


-- | Recognized fragments separated by junk.
-- 
-- Examples:
-- 
-- >>> head $ parse (fragments $ text "A") "..A."
-- [Unrecognized '.',Unrecognized '.',Recognized (),Unrecognized '.']
fragments :: Syntax s => s a -> s [Fragment a]
fragments recognize = many fragment where
  fragment = recognized <$> recognize
         <|> unrecognized <$> token
