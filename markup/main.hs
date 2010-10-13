module Main where

import Text.ParserCombinators.Parsec hiding (newline,tab)
import System.Environment

--newline :: Parser Char
newline = try (string "\r\n" >> return '\n')
    <|> (string "\r" >> return '\n')
    <|> (string "\n" >> return '\n')

--tab :: Parser String
tab = (string "\t") >> (return "        ")


charToString :: Char -> Parser String
charToString x = return [x]

--line :: Parser String
line = do
    contents <- many ((letter >>= charToString) <|> (string " ") <|> tab) 
    newline
    return contents

getlines = many line

parseMarkup :: String -> IO ()
parseMarkup input = case (parse getlines "markup" input) of
    Left err -> print err
    Right vals -> print vals

main :: IO ()
main = do
    input <- readFile "input.mup"
    parseMarkup input

