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

indentationOf (IndentedLine level _ _) = level
markerOf (IndentedLine _ marker _) = marker
contentOf (IndentedLine _ _ content) = content

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

data Element = Paragraph Integer [String]
    | Enumeration [String]

instance Show Element where
    show (Paragraph level strs) = "P(" ++ (show level) ++ ") " ++ (foldl1 (++) $ map show strs)

paragraph :: Parser Element
paragraph = do {
    fl <- line ;
    if (isEndLine fl) then
        fail "EOF"
      else let
          level = (indentationOf fl)
          fc = (contentOf fl)
          paragraph' level ls = let
                  sofar = Paragraph level (reverse ls)
                in (eof >> return sofar) <|> do
                    nl <- line
                    if (isEndLine nl) then
                        return sofar
                     else let
                        nc = (contentOf nl)
                      in paragraph' level (nc:ls)
        in paragraph' level [fc]
    } where
        isEndLine line = (isBlankLine line) || (isEndMarker (markerOf line))
        isEndMarker Enumerate = True
        isEndMarker Itemise = True
        isEndMarker _ = False


getlines :: Parser [Element]
getlines = many paragraph

parseMarkup :: String -> String
parseMarkup input = case (parse getlines "markup" input) of
    Left err -> show err
    Right vals -> show vals
