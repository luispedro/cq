module Markup where

import Text.ParserCombinators.Parsec
import Debug.Trace
tracex x = trace (show x) x

data Element = Paragraph String
    | Verbatim String
    | UList [Element]
    | OList [Element]
    | UListElement [Element]
    | OListElement [Element]
    | Block [Element]

instance Show Element where
    show (Paragraph str) = "<p>" ++ str ++ "</p>"
    show (Verbatim str) = "<pre>" ++ str ++ "</pre>"
    show (UList elems) = "<ul>" ++ (join $map show elems) ++ "</ul>"
    show (UListElement elems) = "<li>" ++ (join $map show elems) ++ "</li>"
    show (OList elems) = "<ol>" ++ (join $map show elems) ++ "</ol>"
    show (OListElement elems) = "<li>" ++ (join $map show elems) ++ "</li>"
    show (Block elems) = "<blockquote>" ++ (join $ map show elems) ++ "</blockquote>"

data Document = Document [Element]

instance Show Document where
    show (Document es) = "<document>" ++ (join $ map show es) ++ "</document>"

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
blockstart = try $ indent 2

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
push_indent n = do
    updateState (\(SimpleIndent x _) -> (SimpleIndent (x+n) True))
pop_indent :: Integer  -> CharParser IndentState ()
pop_indent n = updateState (\(SimpleIndent x s) -> (SimpleIndent (x-n) s))

emptyline :: CharParser IndentState ()
emptyline = (many (char ' ') >> eol >> return ())

indentedline :: CharParser IndentState String
indentedline = do
    curindent
    first <- noneOf " -#*\n\r"
    rest <- many (noneOf "\r\n")
    eol
    return (first:rest)

join [] = ""
join xs = foldr1 (++) xs
paragraph :: CharParser IndentState Element
paragraph = do
    lines <- many1 indentedline
    emptyline
    return $ Paragraph $ join $ map (\x -> (x ++ " ")) lines

verbatimline = (try indentedline)
        <|> (try $ (emptyline >> (return "")))

verbatim = do
    curindent
    verbatimstart
    notFollowedBy (char ' ')
    push_indent 3
    lines <- many indentedline
    pop_indent 3
    return $ Verbatim $ join $ tracex lines

block = do
    curindent
    blockstart
    notFollowedBy (char ' ')
    push_indent 2
    elems <- many element
    pop_indent 2
    return $ Block elems

element :: CharParser IndentState Element
element = do
    (try block)
    <|> (try paragraph)
    <|> (try verbatim)

document :: CharParser IndentState Document
document = do
    elems <- many element
    skipMany ((char '\n') <|> (char ' '))
    eof
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

