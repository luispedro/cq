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


countinstances :: Char -> Parser Integer
countinstances c = countinstances' c 0
    where
        countinstances' c n = (char c >> (countinstances' c (n+1))) <|> (return n)

data StartMarker = NoMarker
    | Header Integer
    | Enumerate
    | Itemise

instance Show StartMarker where
    show (NoMarker) = ""
    show (Header n) = "*" ++ (show n) ++ "*"
    show (Enumerate) = "#"
    show (Itemise) = "-"

data IndentedLine = IndentedLine Integer StartMarker String

instance Show IndentedLine where
    show (IndentedLine level marker str) = "IL(" ++ (show level) ++ ":" ++ (show marker) ++ ")" ++ str

isBlankLine :: IndentedLine -> Bool
isBlankLine (IndentedLine _ _ "") = True
isBlankLine (IndentedLine _ _ _)  = False

startmarker :: Parser StartMarker
startmarker = (char '#' >> return Enumerate)
    <|> (char '-' >> return Itemise)
    <|> (countinstances ' ' >>= \n -> (return $ Header n))

line :: Parser IndentedLine
line = do
    level <- indentation
    marker <- startmarker
    contents <- many (noneOf "\n\r")
    newline
    return $ IndentedLine level marker contents

getlines = many line

parseMarkup :: String -> String
parseMarkup input = case (parse getlines "markup" input) of
    Left err -> show err
    Right vals -> show vals
