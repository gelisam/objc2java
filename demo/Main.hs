import Data.Functor ((<$>))
import qualified Text.Java as Java
import qualified Text.ObjC as ObjC

import Convert
import Text.Fragment


objc2java :: String -> String
objc2java = convert_fragments convert ObjC.expr Java.expr

main :: IO ()
main = do output <- objc2java <$> getContents
          putStr output
