module Parser where

import Text.Parsec
import Text.Parsec.String
import Lib 

textParser :: Parser DataType
textParser = Text <$> (between (char '"') (char '"') (many (noneOf "\"")))

letterParser :: Parser DataType
letterParser = Letter <$> ( between (char '\'') (char '\'') (anyChar) )

noneParser :: Parser DataType
noneParser =  None <$> (spaces <* (string "none"))

numberParser :: Parser DataType
numberParser = Number <$> (many1 digit)

anyValParser :: Parser DataType
anyValParser = textParser <|> letterParser <|> numberParser

labelParser :: Parser String
labelParser = (many1 letter)

isAPArser :: Parser Operation
isAPArser = IsA <$> (labelParser) <*> (many1 space >> string "is a" >> (many1 space >> (string "number" <|> string "text" <|> string "binary")))

isEqParser :: Parser Operation
isEqParser = IsEq <$> (labelParser) <*> (many1 space >> string "is equal to " >> anyValParser)

printParser :: Parser Operation
printParser = Print <$> (string "print" >> many1 space >> anyValParser)

endSuccessParser :: Parser Operation
endSuccessParser = EndS <$> (spaces <* string "end program successfully")

endFailParser :: Parser Operation
endFailParser = EndF <$> (spaces <* string "end program with error")

mainParser :: String -> Either ParseError [Operation]
mainParser s = parse (spaces >> many (((try $ isEqParser) <|> (try $ isAPArser) <|> (try $ printParser) <|> (try $ endSuccessParser) <|> (try $ endFailParser))  <* spaces) <* eof) "example" s
