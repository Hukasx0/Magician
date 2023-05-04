module Main (main) where

import System.Environment
import Data.List

import Parser
import Compile

cBase :: String
cBase = "#include <stdlib.h>\n#include <stdio.h>\n\nint main()\n{"

main :: IO ()
main = do 
    fName <-head <$> getArgs
    fData <- readFile $ fName
    case (mainParser $ fData) of 
        Left err -> (print $ err)
        Right ok -> writeFile "magician_out.c" (cBase ++ (intercalate "\n" $ map (compile) ok) ++ "\n}\n")
