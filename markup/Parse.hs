module Parse where

{- * General parsing strategy
 -
 - ** Character normalisations
 -
 - Transformations such as("\r\n" -> '\n' & '\t' -> "        ")
 - and modeline removal are done as a preprocessing step
 - (Haskell's laziness is perfect for this code)
 -
 - ** Parsing
 -
 - Markup parsing is done with Parsec
 - 
 - The parser needs to rememember the state: which is the current indentation
 - level and whether we are in a nested tag. This is done with parser state as
 - it naturally maps to the Monad abstraction.
 - 
 - ** Interface
 - 
 - Main function is parseMarkup which is String -> Either ParseError Document
 -}

import Markup
import Text.ParserCombinators.Parsec

{- IndentState saves the state of the parser
 -   (nr of indent level measured in spaces)
 -   (whether we have already consumed the current line's indentation)
 -   (nesting level [for \note{} style tags]
 -}
data IndentState = IndentState  { indentLevel :: Integer
                                , ignoreNext :: Bool
                                , nestLevel :: Integer } deriving (Show)

noIgnoreNext st = st {ignoreNext = False}
push_indent n = updateState $ push_indent' n
    where push_indent' n st = st { ignoreNext = True, indentLevel = (+n) $ indentLevel st}
pop_indent n = updateState $ pop_indent' n
    where pop_indent' n st = st { indentLevel = (+ (-n)) $ indentLevel st}
push_state = updateState push_state'
    where push_state' st = st { ignoreNext = True, nestLevel = (+1) $ nestLevel st}
pop_state = updateState pop_state'
    where pop_state' st = st { nestLevel = (+ (-1)) $ nestLevel st}


--
-- Parsing
--

eol = (char '\n')
eofl = do -- end of file or line
    st <- getState
    if (nestLevel st) > 0 then
        eol <|> ((lookAhead (char '}')) >> return '\n')
     else
        eol <|> (eof >> return '\n')

headermarker :: CharParser IndentState Integer
headermarker = (char '*' >> headermarker' 1)
    where headermarker' n = (char '*' >> (headermarker' (n+1))) <|> (char ' ' >> (return n)) <|> (return n) -- allow for "**Header" to parse correctly


oliststart = try $ string "  # "
uliststart = try $ string "  - "
verbatimstart = try $ string "   "
blockstart = try $ string "  "

curindent :: CharParser IndentState ()
curindent = try $ do
        st <- getState
        curindent' st
    where
    curindent' :: IndentState -> CharParser IndentState ()
    curindent' (IndentState  _ True _) = updateState noIgnoreNext
    curindent' (IndentState  n False _) = (count (fromInteger n) (char ' ')) >> (return ())


emptyline :: CharParser IndentState ()
emptyline = try $ (many (char ' ')) >> eol >> return ()
skipemptylines = skipMany emptyline

indentedline :: CharParser IndentState Text
indentedline = do
    curindent
    t <- text
    return t


rawtext :: CharParser IndentState Text
rawtext = do
    st <- getState
    if (nestLevel st) > 0 then
        many1 (noneOf "}[\\\n") >>= (return . RawText)
     else
        many1 (noneOf "[\\\n") >>= (return . RawText)

rawtextinline :: CharParser IndentState Text
rawtextinline = many1 (noneOf "\\\n}") >>= (return . RawText)

tagname :: CharParser IndentState String
tagname = many1 (letter <|> (char '_') <|> (char '.') <|> (char '+'))

charToStr c = [c]

taggedtext :: CharParser IndentState Text
taggedtext = do
    (char '\\')
    ((oneOf "\\{}[]#-*") >>= (return . RawText . charToStr)) <|> do -- if it is followed by a \, then it is an escaped \
        tag <- tagname
        char '{'
        if tag `elem` blockTags then do
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

escapedchar :: CharParser IndentState Text
escapedchar = (char '\\') >> (oneOf "\\#-") >>= (return . RawText . charToStr)

linktext = do
    char '['
    txt <- many (noneOf "|]\n")
    ((char ']') >> (return $ Link txt)) <|> do
        char '|'
        key <- many (noneOf "]\n")
        char ']'
        return $ LinkWKey txt key


text :: CharParser IndentState Text
text = do
    lookAhead $ noneOf "* \n"
    first <- (try escapedchar) <|> (return $ RawText "")
    txt <- many1 (taggedtext <|> linktext <|> rawtext)
    next <- (char '\n' >> (return $ RawText " ")) <|> (return $ RawText "")
    return $ Sequence ((first:txt)++[next])



paragraph :: CharParser IndentState Element
paragraph = do
    notFollowedBy (linkdef >> eofl)
    lines <- many1 $ try indentedline
    optional emptyline
    return $ Paragraph $ Sequence lines

verbatimline = do {
            curindent
          ; vtext <- manyTill anyChar eofl
          ; return (vtext ++ "\n")
        } <|> (emptyline >> (return "\n"))

verbatim = do
    verbatimstart
    push_indent 3
    lines <- many verbatimline
    pop_indent 3
    return $ Verbatim $ concat lines


metablock n starter constructor = do
    starter
    push_indent n
    elems <- many1 (skipemptylines >> element)
    pop_indent n
    return $ constructor elems

block = metablock 2 blockstart Block
olistelem = metablock 4 oliststart OListElement
ulistelem = metablock 4 uliststart UListElement

olist = do
    first <- olistelem
    rest <- many (skipemptylines >> curindent >> olistelem)
    return $ OList (first:rest)
ulist = do
    first <- ulistelem
    rest <- many (skipemptylines >> curindent >> ulistelem)
    return $ UList (first:rest)

header = do
    n <- headermarker
    rest <- manyTill anyChar eofl
    return $ Header n rest

linkdef = do
    char '['
    key <- many (noneOf "]\n")
    char ']'
    skipMany (char ' ')
    char '<'
    url <- many (noneOf ">")
    char '>'
    skipMany (char ' ')
    eofl
    return $ LinkDef key url

element :: CharParser IndentState Element
element = do
    (try paragraph)
    <|> do
        curindent
        header <|> olist <|> ulist <|> linkdef <|> verbatim <|> block


document :: CharParser IndentState Document
document = do
    elems <- many $ skipemptylines >> element
    skipemptylines >> eof
    return $ Document elems


--
-- Preprocessing
--

preprocess input = concat $ map tabTo8 $ removeModeline $ fixNLs input
    where
    tabTo8 '\t' = "        "
    tabTo8 c = [c]
    fixNLs [] = []
    fixNLs ('\r':'\n':xs) = ('\n':fixNLs xs)
    fixNLs ('\n':'\r':xs) = ('\n':fixNLs xs)
    fixNLs (x:xs) = (x:fixNLs xs)

removeModeline :: String -> String
removeModeline = removeModeline' True 0
    where
    removeModeline' False _ ('\n':xs) = ('\n':removeModeline' True 0 xs)
    removeModeline' False _ (x:xs) = (x:removeModeline' False 0 xs)
    removeModeline' True n (x:xs) = if x /= (modeline !! n) then
                    (take n modeline) ++ [x] ++ (removeModeline' (x == '\n') 0 xs)
                    else (if (n+1)== length modeline then removeModeline' True 0 xs else removeModeline' True (n+1) xs)
    removeModeline' _ _ [] = []
    modeline = "-*- mode: markup; -*-\n"

--
-- Entry Point
--

parseMarkup :: String -> Either ParseError Document
parseMarkup input = runParser document (IndentState  0 False 0) "markup" $ preprocess input

