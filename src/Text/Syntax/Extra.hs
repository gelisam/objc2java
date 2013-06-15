-- | A few useful tools when constructing invertible syntaxes.
module Text.Syntax.Extra where

import Prelude ()

import Control.Category ((.))
import Control.Isomorphism.Partial
import Text.Syntax
import Text.Syntax.Parser.Naive (parse)

import Control.Isomorphism.Partial.Prim.Extra (snd)
import Control.Isomorphism.Partial.Constructors.Extra (cdr)


-- | A heterogeneous version of @chainl1@.
-- 
-- >>> parse (chainl1 (text "A") (text ",") (ignore ((), ((), ())))) "A,A,A"
-- [()]
-- >>> parse (chainl1 (text "A") (text ",") (ignore ((), ((), ())))) "A,B,B"
-- []
-- >>> parse (chainl1' (text "A") (text ",") (text "B") (ignore ((), ((), ())))) "A,A,A"
-- []
-- >>> parse (chainl1' (text "A") (text ",") (text "B") (ignore ((), ((), ())))) "A,B,B"
-- [()]
chainl1' :: Syntax s => s a -> s b -> s c -> Iso (a, (b, c)) a -> s a
chainl1' arg0 op arg f = foldl f <$> arg0 <*> many (op <*> arg)

-- | A heterogeneous version of @sepBy@.
-- 
-- >>> parse (sepBy (text "A") (text ",")) "A,A,A"
-- [[(),(),()]]
-- >>> parse (sepBy (text "A") (text ",")) "A,B,B"
-- []
-- >>> parse (sepBy' (text "A") (text ",") (text "B") (ignore ((), ()))) "A,A,A"
-- []
-- >>> parse (sepBy' (text "A") (text ",") (text "B") (ignore ((), ()))) "A,B,B"
-- [()]
sepBy' :: Syntax s => s a -> s () -> s b -> Iso (a, b) a -> s a
sepBy' arg0 op arg f = chainl1' arg0 op arg (f . drop_op)
                       where
  drop_op :: Iso (a, ((), b)) (a, b)
  drop_op = snd cdr

-- | A non-empty version of @sepBy@.
-- 
-- >>> parse (sepBy (text "A") (text ",")) "A"
-- [[()]]
-- 
-- >>> parse (sepBy (text "A") (text ",")) ""
-- [[]]
-- 
-- >>> parse (sepBy1 (text "A") (text ",")) "A"
-- [[()]]
-- 
-- >>> parse (sepBy1 (text "A") (text ",")) ""
-- []
sepBy1 :: Syntax delta => delta alpha -> delta () -> delta [alpha]
sepBy1 arg op = cons <$> arg <*> many (op *> arg) 
