import Data.Functor ((<$>))
import Data.List (intercalate)
import Data.Maybe (fromJust)
import qualified Text.Java as Java
import qualified Text.ObjC as ObjC
import qualified Text.Syntax.Parser.Naive as Parser (parse)
import qualified Text.Syntax.Printer.Naive as Printer (print)

import Text.Fragment


testConvert :: String -> Maybe String
testConvert input = do
  [ast] <- Just (Parser.parse ObjC.expr input)
  Printer.print Java.expr (convert ast)

-- | Convert ObjectiveC idioms into corresponding Java idioms.
-- 
-- Examples:
-- 
-- >>> testConvert "myData"
-- Just "myData"
-- 
-- >>> testConvert "[[Hello alloc] init]"
-- Just "new Hello()"
-- 
-- >>> testConvert "[myData writeToFile: @\"/tmp/log.txt\" atomically: NO]"
-- Just "myData.writeToFile_atomically(\"/tmp/log.txt\", false)"
convert :: ObjC.Expr -> Java.Expr
convert (ObjC.Var "YES")         = Java.Var "true"
convert (ObjC.Var "NO")          = Java.Var "false"
convert (ObjC.MethodCall (ObjC.MethodCall (ObjC.Var class_name)
                                          ["alloc"]
                                          [])
                         (init:_)
                         args) | take 4 init == "init"
  = Java.ConstructorCall class_name (fmap convert args)
-- INSERT MORE IDIOMS ABOVE THIS LINE

-- generic cases last
convert (ObjC.Var x)             = Java.Var x
convert (ObjC.StringLit s)       = Java.StringLit s
convert (ObjC.FunctionCall f xs) = Java.FunctionCall f (fmap convert xs)
convert (ObjC.MethodCall target
                         method_name_parts
                         args)
  = Java.MethodCall (convert target)
                    java_method_name
                    (fmap convert args)
    where
  java_method_name = intercalate "_" method_name_parts


objc2java :: String -> String
objc2java = fromJust
          . Printer.print (fragments Java.expr)
          . (fmap . fmap) convert
          . head
          . Parser.parse (fragments ObjC.expr)

main :: IO ()
main = do output <- objc2java <$> getContents
          putStr output
