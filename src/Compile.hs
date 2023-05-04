module Compile (compile) where

import Lib
import Parser 

type CCode = String

tokensToC :: Operation -> CCode
tokensToC (IsA name ds) = (valNameToC $ ds) ++ " " ++ name ++ ";"
tokensToC (IsEq name val) = name ++ " = " ++ (valToC $ val) ++ ";"
tokensToC (EndS _) = "exit(0);"
tokensToC (EndF _) = "exit(1);"
tokensToC (Print (Text s)) = "printf(\""++s++"\\n\");"
tokensToC (Print (Number n)) = "printf(\"%d\\n\","++n++");"
tokensToC (Print (Letter l)) = "printf(\"%c\\n\","++[l]++");"

valNameToC :: String -> String
valNameToC "number" = "int"
valNameToC "text" = "char*"

valToC :: DataType -> String
valToC (Text t) = "\""++t++"\""
valToC (Number n) = n

compile :: Operation -> String
compile str = tokensToC $ str
