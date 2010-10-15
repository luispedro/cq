module Markup where

import Text.ParserCombinators.Parsec
import XML (xmlShow)
import Debug.Trace
tracex x = trace (show x) x

data Text = RawText String
    | LinkText String String -- link key
    | InlineTag String String -- tag str
    | BlockTag String String -- tag str
    | Sequence [Text]

data Element = Paragraph String
    | Verbatim String
    | UList [Element]
    | OList [Element]
    | UListElement [Element]
    | OListElement [Element]
    | Block [Element]
    | Header Integer String

instance Show Element where
    show = show' 0
        where
        show' n (Paragraph str) = xmlShow n "p" str
        show' n (Verbatim str) = xmlShow n "pre" str
        show' n (UList elems) = xmlShow n "ul" (join $ map (show' (n+1)) elems)
        show' n (UListElement elems) = xmlShow n "li" (join $map (show' (n+1)) elems)
        show' n (OList elems) = xmlShow n "ol" (join $map (show' (n+1)) elems)
        show' n (OListElement elems) = xmlShow n "li" (join $map (show' (n+1)) elems)
        show' n (Block elems) = xmlShow n "blockquote" (join $ map (show' (n+1)) elems)
        show' n (Header hl str) = xmlShow n ("h" ++ (show hl)) str

data Document = Document [Element]

instance Show Document where
    show (Document es) = "<document>" ++ (join $ map show es) ++ "</document>"

data IndentState = SimpleIndent Integer Bool

eol = try (string "\r\n" >> return '\n')
    <|> (string "\r" >> return '\n')
    <|> (string "\n" >> return '\n')

eofl = eol  <|> (eof >> return '\n') -- This always terminates the last line in the file


charToString :: Char -> Parser String
charToString x = return [x]

headermarker :: CharParser IndentState Integer
headermarker = (char '*' >> headermarker' 1)
    where headermarker' n = (char '*' >> headermarker' (n+1)) <|> (return n)

indent :: Integer -> CharParser IndentState ()
indent 0 = return ()
indent n = (char ' ' >> (indent (n-1)))

oliststart :: CharParser IndentState ()
oliststart = try $ (string "# ") >> return ()
uliststart :: CharParser IndentState ()
uliststart = try $ (string "- ") >> return ()
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
emptyline = (many (char ' ')) >> eol >> return ()

indentedline :: CharParser IndentState String
indentedline = do
    curindent
    first <- noneOf " -#*\n\r"
    rest <- text
    eofl
    return (first:rest)

text = many (noneOf "\r\n")

join [] = ""
join xs = foldr1 (++) xs
paragraph :: CharParser IndentState Element
paragraph = do
    lines <- many1 indentedline
    optional emptyline
    return $ Paragraph $ join $ map (\x -> (x ++ " ")) lines

verbatimline = (try indentedline)
        <|> (try $ (emptyline >> (return "")))

verbatim = do
    verbatimstart
    notFollowedBy (char ' ')
    push_indent 3
    lines <- many indentedline
    pop_indent 3
    return $ Verbatim $ join lines


metablock starter constructor = do
    starter
    notFollowedBy (char ' ')
    push_indent 2
    elems <- many element
    pop_indent 2
    return $ constructor  elems

block = metablock blockstart Block
olistelem = metablock oliststart OListElement
ulistelem = metablock uliststart UListElement

olist = (many1 olistelem) >>= (return . OList)
ulist = (many1 ulistelem) >>= (return . UList)

header = do
    n <- headermarker
    rest <- many (noneOf "\r\n")
    eofl
    return $ Header n rest

element :: CharParser IndentState Element
element = do
    many (try emptyline)
    (try paragraph)
    <|> do
        curindent
        header <|> olist <|> ulist <|> verbatim <|> block


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

