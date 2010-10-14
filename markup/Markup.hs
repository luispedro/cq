module Markup where

import Text.ParserCombinators.Parsec

eol = try (string "\r\n" >> return '\n')
    <|> (string "\r" >> return '\n')
    <|> (string "\n" >> return '\n')
    <|> (eof >> return '\n') -- This always terminates the last line in the file


charToString :: Char -> Parser String
charToString x = return [x]

headermarker :: Parser Integer
headermarker = (char '*' >> headermarker' 1)
    where headermarker' n = (char '*' >> headermarker' (n+1)) <|> (return n)

indent :: Integer -> CharParser Integer ()
indent 0 = return ()
indent n = (char ' ' >> (indent (n-1)))

enumeratestart :: CharParser Integer ()
enumeratestart = (string "# ") >> return ()
itemsstart :: CharParser Integer ()
itemsstart = (string "- ") >> return ()
verbatimstart = try $ indent 3
blockstart = indent 2

curindent :: CharParser Integer ()
curindent = (getState >>= indent)

push_indent :: Integer -> CharParser Integer ()
push_indent n = updateState (+n)
pop_indent :: Integer -> CharParser Integer ()
pop_indent n = updateState (\x -> (x-n))

emptyline :: CharParser Integer ()
emptyline = (many (char ' ') >> eol >> return ())

indentedline = do
    curindent
    first <- noneOf " -#*"
    rest <- manyTill anyChar eol
    return (first:rest)

join = foldr1 (++)
tracex x = trace (show x) x

paragraph :: CharParser Integer String
paragraph = do
    lines <- many indentedline
    emptyline
    return $ join lines

block = do
    blockstart
    push_indent 2
    elems <- many element
    pop_indent 2
    return elems

element = paragraph

--paragraphs :: CharParser Integer [String]
--paragraphs = many paragraph

preprocess input = join $ map tabTo8 input
    where
        tabTo8 '\t' = "        "
        tabTo8 c = [c]

parseMarkup :: String -> String
parseMarkup input =
    case (runParser paragraph 0 "markup" $ preprocess input) of
        Left err -> show err
        Right vals -> show vals

