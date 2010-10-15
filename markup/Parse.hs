module Parse where

import Markup
import Text.ParserCombinators.Parsec

data IndentState = SimpleIndent Integer Bool Integer
eol = try (string "\r\n" >> return '\n')
    <|> (char '\r') <|> (char '\n')

eofl = do
    st <- getState
    if (nestLevelOf st) == 0 then
        eol  <|> (eof >> return '\n') -- This always terminates the last line in the file
     else
        eol <|> (lookAhead (char '}'))


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

push_state = updateState push_state'
    where
    push_state' (SimpleIndent i _ n) = SimpleIndent i True (n+1)
pop_state = updateState pop_state'
    where
    pop_state' (SimpleIndent i _ n) = SimpleIndent i True (n-1)
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
    if nestLevelOf st == 0 then
        many1 (noneOf "\\\n\r") >>= (return . RawText)
     else
        many1 (noneOf "}\\\n\r") >>= (return . RawText)

rawtextinline :: CharParser IndentState Text
rawtextinline = many1 (noneOf "\\\n\r}") >>= (return . RawText)

tagname = many1 (letter <|> (char '_') <|> (char '.') <|> (char '+'))
taggedtext :: CharParser IndentState Text
taggedtext = do
    (char '\\')
    tag <- tagname
    char '{'
    if tag == "note" then do
        push_state
        par <- paragraph
        elems <- many element
        char '}'
        pop_state
        return $ BlockTag tag (par:elems)
     else do
        Sequence content <- inlinetext
        char '}'
        return $ InlineTag tag content

inlinetext :: CharParser IndentState Text
inlinetext = many (rawtextinline <|> taggedtext) >>= (return . Sequence)

text :: CharParser IndentState Text
text = do
    lookAhead $ noneOf " -#*\n\r"
    txt <- many1 (taggedtext <|> rawtext)
    return $ Sequence txt


paragraph :: CharParser IndentState Element
paragraph = do
    lines <- many1 $ try indentedline
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
    return $ Verbatim $ concat lines


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

preprocess input = concat $ map tabTo8 $ removeModeline input
    where
        tabTo8 '\t' = "        "
        tabTo8 c = [c]
        removeModeline = removeModeline' True 0
        removeModeline' False _ ('\n':xs) = ('\n':removeModeline' True 0 xs)
        removeModeline' False _ ('\r':xs) = ('\n':removeModeline' True 0 xs)
        removeModeline' False _ (x:xs) = (x:removeModeline' False 0 xs)
        removeModeline' True n (x:xs) = if x /= (modeline !! n) then
                        (take n modeline) ++ [x] ++ (removeModeline' (x `elem` "\r\n") 0 xs)
                        else (if n == length modeline then removeModeline' True 0 xs else removeModeline' True (n+1) xs)
        removeModeline' _ _ [] = []
        modeline = "-*- mode: markup; -*-\n"

parseMarkup :: String -> String
parseMarkup input =
    case (runParser document (SimpleIndent 0 False 0) "markup" $ preprocess input) of
        Left err -> show err
        Right vals -> show vals
