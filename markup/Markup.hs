module Markup where

import Text.ParserCombinators.Parsec

data Element = Paragraph String
    | Verbatim String
    | Enumeration [Element]
    | Items [Element]
    | Block [Element]

instance Show Element where
    show (Paragraph str) = "<p>" ++ str ++ "</p>"
    show (Verbatim str) = "<pre>" ++ str ++ "</pre>"
    show (Enumeration elems) = "<ol>" ++ (join $map show elems) ++ "</ol>"
    show (Items elems) = "<ul>" ++ (join $ map show elems) ++ "</ul>"
    show (Block elems) = "<blockquote>" ++ (join $ map show elems) ++ "</blockquote>"

data Document = Document [Element]

instance Show Document where
    show (Document es) = join $ map show es

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

indentedline :: CharParser Integer String
indentedline = do
    curindent
    first <- noneOf " -#*\n\r"
    rest <- many (noneOf "\r\n")
    eol
    return (first:rest)

join = foldr1 (++)
paragraph :: CharParser Integer Element
paragraph = do
    lines <- many indentedline
    emptyline
    return $ Paragraph $ sepjoin " " lines

sepjoin sep strs = sepjoin' sep strs ""
    where
    sepjoin' _ [] s = s
    sepjoin' sep (x:xs) s = sepjoin' sep xs (s ++ sep ++ x)

block = do
    blockstart
    push_indent 2
    elems <- many element
    pop_indent 2
    return elems

element :: CharParser Integer Element
element = paragraph

document :: CharParser Integer Document
document = do
    elems <- many element
    return $ Document elems

preprocess input = join $ map tabTo8 input
    where
        tabTo8 '\t' = "        "
        tabTo8 c = [c]

parseMarkup :: String -> String
parseMarkup input =
    case (runParser paragraph 0 "markup" $ preprocess input) of
        Left err -> show err
        Right vals -> show vals

