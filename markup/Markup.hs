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

data IndentState = SimpleIndent Integer Bool

eol = try (string "\r\n" >> return '\n')
    <|> (string "\r" >> return '\n')
    <|> (string "\n" >> return '\n')
    <|> (eof >> return '\n') -- This always terminates the last line in the file


charToString :: Char -> Parser String
charToString x = return [x]

headermarker :: CharParser IndentState Integer
headermarker = (char '*' >> headermarker' 1)
    where headermarker' n = (char '*' >> headermarker' (n+1)) <|> (return n)

indent :: Integer -> CharParser IndentState ()
indent 0 = return ()
indent n = (char ' ' >> (indent (n-1)))

enumeratestart :: CharParser IndentState ()
enumeratestart = (string "# ") >> return ()
itemsstart :: CharParser IndentState ()
itemsstart = (string "- ") >> return ()
verbatimstart = try $ indent 3
blockstart = indent 2

ignore :: IndentState -> CharParser IndentState ()
ignore (SimpleIndent _ True) = updateState noIgnoreNext
    where
    noIgnoreNext (SimpleIndent n True) = SimpleIndent n False
ignore (SimpleIndent n False) = indent n

curindent :: CharParser IndentState ()
curindent = try $ do
    st <- getState
    ignore st

push_indent :: Integer -> CharParser IndentState ()
push_indent n = updateState (\(SimpleIndent x s) -> (SimpleIndent (x+n) s))
pop_indent :: Integer  -> CharParser IndentState ()
pop_indent n = updateState (\(SimpleIndent x s) -> (SimpleIndent (x-n) s))
ignore_next = updateState (\(SimpleIndent x _) -> (SimpleIndent x True))

emptyline :: CharParser IndentState ()
emptyline = (many (char ' ') >> eol >> return ())

indentedline :: CharParser IndentState String
indentedline = do
    curindent
    first <- noneOf " -#*\n\r"
    rest <- many (noneOf "\r\n")
    eol
    return (first:rest)

join = foldr1 (++)
paragraph :: CharParser IndentState Element
paragraph = do
    lines <- many1 indentedline
    emptyline
    return $ Paragraph $ join $ map (\x -> (x ++ " ")) lines

block = do
    blockstart
    push_indent 2
    ignore_next
    elems <- many element
    pop_indent 2
    return $ Block elems

element :: CharParser IndentState Element
element = paragraph
    <|> block

document :: CharParser IndentState Document
document = do
    elems <- many element
    return $ Document elems

preprocess input = join $ map tabTo8 input
    where
        tabTo8 '\t' = "        "
        tabTo8 c = [c]

parseMarkup :: String -> String
parseMarkup input =
    case (runParser document (SimpleIndent 0 False) "markup" $ preprocess input) of
        Left err -> show err
        Right vals -> show vals

