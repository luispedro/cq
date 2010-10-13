module Main where

import Text.ParserCombinators.Parsec hiding (newline,tab)
import System.Environment

newline :: Parser Char
newline = try (string "\r\n" >> return '\n')
    <|> (string "\r" >> return '\n')
    <|> (string "\n" >> return '\n')


charToString :: Char -> Parser String
charToString x = return [x]


indentation :: Parser Integer
indentation = indentation' 0
    where
        indentation' n =
            (char '\t' >> (indentation' (n+8)))
            <|> (char ' ' >> (indentation' (n+1)))
            <|> (return n)


data IndentedLine = IndentedLine Integer String


instance Show IndentedLine where
    show (IndentedLine level str) = "IL(" ++ (show level) ++ "):" ++ str

line :: Parser IndentedLine
line = do
    level <- indentation
    contents <- many ((letter >>= charToString) <|> (string " "))
    newline
    return $ IndentedLine level $ foldl (++) "" contents

getlines = many line

parseMarkup :: String -> IO ()
parseMarkup input = case (parse getlines "markup" input) of
    Left err -> print err
    Right vals -> print vals

main :: IO ()
main = do
    input <- readFile "input.mup"
    parseMarkup input

