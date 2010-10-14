module Markup where

import Text.ParserCombinators.Parsec hiding (newline,tab)


newline :: Parser Char
newline = try (string "\r\n" >> return '\n')
    <|> (string "\r" >> return '\n')
    <|> (string "\n" >> return '\n')
    <|> (eof >> return '\n') -- This always terminates the last line in the file


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

isBlankLine :: IndentedLine -> Bool
isBlankLine (IndentedLine _ "") = True
isBlankLine (IndentedLine _ _)  = False

line :: Parser IndentedLine
line = do
    level <- indentation
    contents <- many (noneOf "\n\r")
    newline
    return $ IndentedLine level contents

getlines = many line

parseMarkup :: String -> String
parseMarkup input = case (parse getlines "markup" input) of
    Left err -> show err
    Right vals -> show vals
