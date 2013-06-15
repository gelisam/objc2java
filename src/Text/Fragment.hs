{-# LANGUAGE DeriveFunctor, TemplateHaskell #-}
-- | Incomplete grammar? Leave the unrecognized parts alone.
module Text.Fragment where

import Control.Isomorphism.Partial ((<$>))
import Control.Isomorphism.Partial.TH (defineIsomorphisms)
import Data.Maybe (fromJust)
import Text.Syntax
import qualified Text.Syntax.Parser.Naive as Parser
import qualified Text.Syntax.Printer.Naive as Printer


data Fragment a = Recognized a
                | Unrecognized Char
     deriving (Show, Eq, Functor)

$(defineIsomorphisms ''Fragment)


-- | Recognized fragments separated by junk.
-- 
-- >>> head $ Parser.parse (fragments $ text "A") "..A."
-- [Unrecognized '.',Unrecognized '.',Recognized (),Unrecognized '.']
fragments :: Syntax s => s a -> s [Fragment a]
fragments recognize = many fragment where
  fragment = recognized <$> recognize
         <|> unrecognized <$> token


-- | Convert recognized fragments, leaving the rest alone.
-- 
-- >>> convert_fragments id (text "A") (text "B") "..A."
-- "..B."
convert_fragments :: (a -> b)
                  -> Parser.Parser a -> Printer.Printer b
                  -> String -> String
convert_fragments convert parser printer
  = fromJust . Printer.print (fragments printer)
  . (fmap . fmap) convert
  . head . Parser.parse (fragments parser)
