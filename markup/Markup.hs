module Markup where

import Text.ParserCombinators.Parsec
import XML (xmlShow)
import Debug.Trace
tracex x = trace (show x) x

data Text = RawText String
    | LinkText String String -- link key
    | InlineTag String [Text] -- tag str
    | BlockTag String [Element] -- tag str
    | Sequence [Text]

instance Show Text where
    show (RawText str) = str
    show (InlineTag tag elems) = xmlShow 0 tag $ join $ map show elems
    show (BlockTag tag elems) = xmlShow 0 tag $ join $ map show elems
    show (Sequence elems) = join $ map show elems

data Element = Paragraph Text
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
        show' n (Paragraph txt) = xmlShow n "p" (show txt)
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

data IndentState = SimpleIndent Integer Bool Integer

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


curindent :: CharParser IndentState ()
curindent = try $ do
        st <- getState
        ignore st
    where
        ignore :: IndentState -> CharParser IndentState ()
        ignore (SimpleIndent _ True _) = updateState noIgnoreNext
            where
            noIgnoreNext (SimpleIndent n True lv) = SimpleIndent n False lv
        ignore (SimpleIndent n False _) = indent n

push_indent :: Integer -> CharParser IndentState ()
push_indent n = do
    updateState (\(SimpleIndent x _ lv) -> (SimpleIndent (x+n) True lv))
pop_indent :: Integer  -> CharParser IndentState ()
pop_indent n = updateState (\(SimpleIndent x s lv) -> (SimpleIndent (x-n) s lv))

push_state (SimpleIndent i _ n) = SimpleIndent i True (n+1)
pop_state (SimpleIndent i _ n) = SimpleIndent i True (n-1)
nestLevelOf (SimpleIndent _ _ n) = n

emptyline :: CharParser IndentState ()
emptyline = (many (char ' ')) >> eol >> return ()

indentedline :: CharParser IndentState Text
indentedline = do
    curindent
    t <- text
    eofl
    return t


rawtext :: CharParser IndentState Text
rawtext = do
    st <- getState
    if nestLevelOf st > 0 then
        many1 (noneOf "\\\n\r") >>= (return . RawText)
     else
        many1 (noneOf "}\\\n\r") >>= (return . RawText)

rawtextinline :: CharParser IndentState Text
rawtextinline = many1 (noneOf "\\\n\r}") >>= (return . RawText)

tagname = many1 (letter <|> (char '_') <|> (char '.') <|> (char '+'))
inlinenote :: CharParser IndentState Text
inlinenote = do
    (char '\\')
    tag <- tagname
    char '{'
    Sequence content <- inlinetext
    char '}'
    return $ InlineTag tag content

inlinetext :: CharParser IndentState Text
inlinetext = many (rawtextinline <|> inlinenote) >>= (return . Sequence)

text :: CharParser IndentState Text
text = do
    first <- noneOf " -#*\n\r"
    rest <- many (inlinenote <|> rawtext)
    return $ Sequence (RawText [first]:rest)


join [] = ""
join xs = foldr1 (++) xs
paragraph :: CharParser IndentState Element
paragraph = do
    lines <- many1 indentedline
    optional emptyline
    return $ Paragraph $ Sequence lines

verbatimline = do {
            curindent
          ; text <- many (noneOf "\r\n")
          ; eofl
          ; return text
        } <|> (try $ (emptyline >> (return "")))

verbatim = do
    verbatimstart
    notFollowedBy (char ' ')
    push_indent 3
    lines <- many verbatimline
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
    case (runParser document (SimpleIndent 0 False 0) "markup" $ preprocess input) of
        Left err -> show err
        Right vals -> show vals

