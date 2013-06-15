-- | Invertible environment lookup.
module Data.Pattern.Env where

import Prelude (Eq, String, Maybe (..))

import Control.Category ((.))
import Control.Isomorphism.Partial.Prim
import Control.Isomorphism.Partial.Constructors (cons, listCases)
import Text.Syntax.Classes (Alternative (..), ProductFunctor (..))

import Control.Isomorphism.Partial.Constructors.Extra (cdr, swap)
import Control.Isomorphism.Partial.Prim.Extra (equals, fst, snd)
import Control.Isomorphism.Partial.Test (testIso)


-- | Whether we are assigning values to variables or observing which variable
--   matches which sub-expression, we always represent the association of
--   variables to values using a simple association list.
type Env e = [(String, e)]

-- | Lookup is invertible if it consumes its value. Returns the looked up value
--   and the reduced environment.
-- 
-- >>> testIso [("a",1), ("b",2), ("c",3)] (lookup "a") (1, [("b",2), ("c",3)])
-- True
-- 
-- The inverse of a lookup is to add the value back inside the list. The value
-- is always added to the beginning of the list, so you might not recover the
-- exact original array.
-- 
-- >>> apply (lookup "c") [("a",1), ("b",2), ("c",3)]
-- Just (3,[("a",1),("b",2)])
-- 
-- >>> unapply (lookup "c") (3, [("a",1), ("b",2)])
-- Just [("c",3),("a",1),("b",2)]
-- 
-- And of course, if the key isn't found in the environment, the lookup fails.
-- 
-- >>> apply (lookup "z") [("a",1), ("b",2), ("c",3)]
-- Nothing
lookup :: String -> Iso (Env a) (a, Env a)
lookup s = (empty ||| consCase) . inverse listCases where
  consCase :: Iso ((String, a), Env a) (a, Env a)
  consCase = (cadr . caar_matches s) <|> recur
  
  caar_matches = fst.fst.equals
  
  cadr :: Iso (((), a), Env a) (a, Env a)
  cadr = cdr . inverse associate
  
  recur :: Iso ((String, a), Env a) (a, Env a)
  recur = snd cons . swap . snd (lookup s)
