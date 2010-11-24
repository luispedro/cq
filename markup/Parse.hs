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
 -
 - Another function parseMarkupWithOptions allows the user to specify whether
 - to use links and which tags should be considered block level. parseMarkup is
 - defined as
 -
 -    parseMarkup str = parseMarkupWithOptions (ParseOptions True ["note"]) str
 - 
 - i.e., \b{use links} and consider \i{note} to be block level.
 -}

import Markup
import Text.ParserCombinators.Parsec

data ParseOptions = ParseOptions { useLinks :: Bool      -- Whether to use links
                                 , blockTags :: [String] -- List of tags that should be block level
                                 } deriving (Show)

data IndentState = IndentState  { indentLevel :: Integer -- Nr of spaces to indent
                                , ignoreNext :: Bool     -- Whether to ignore next call
                                , nestLevel :: Integer   -- Nesting level (for \note{} style tags)
                                , parseOptions :: ParseOptions
                                } deriving (Show)

noIgnoreNext st = st {ignoreNext = False}
push_indent n = updateState $ push_indent' n
    where push_indent' n st = st { ignoreNext = True, indentLevel = (+n) $ indentLevel st}
pop_indent n = updateState $ pop_indent' n
    where pop_indent' n st = st { indentLevel = (+ (-n)) $ indentLevel st}
push_state = updateState push_state'
    where push_state' st = st { nestLevel = (+1) $ nestLevel st}
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
curindent = try $ getState >>= curindent'
    where
    curindent' :: IndentState -> CharParser IndentState ()
    curindent' (IndentState  _ True _ _) = updateState noIgnoreNext
    curindent' (IndentState  n False _ _) = (count (fromInteger n) (char ' ')) >> (return ())


emptyline :: CharParser IndentState ()
emptyline = try $ (many (char ' ')) >> eol >> return ()
skipemptylines = skipMany emptyline



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
        char '{' <?> "tagged text open marker '{'"
        st <- getState
        if tag `elem` (blockTags $ parseOptions st) then do
            push_state
            par <- paragraph
            elems <- many element
            char '}' <?> "tagged text close marker '}'"
            pop_state
            return $ BlockTag tag (par:elems)
         else do
            Sequence content <- inlinetext
            char '}' <?> "tagged text close marker '}'"
            return $ InlineTag tag content

inlinetext :: CharParser IndentState Text
inlinetext = many (rawtextinline <|> taggedtext) >>= (return . Sequence)

escapedchar :: CharParser IndentState Text
escapedchar = (char '\\') >> (oneOf "\\#-") >>= (return . RawText . charToStr)

linktext = do
    st <- getState
    if (not $ useLinks (parseOptions st)) then
        fail "Link parsing not active"
     else do
        char '['
        txt <- many (noneOf "|]\n")
        ((char ']') >> (return $ Link txt)) <|> do
            char '|'
            key <- many (noneOf "]\n")
            char ']'
            return $ LinkWKey txt key


text :: CharParser IndentState [Text]
text = do
    notFollowedBy eofl
    lookAhead $ noneOf "* \n"
    first <- (try escapedchar) <|> (return $ RawText "")
    rest <- many (taggedtext <|> linktext <|> rawtext)
    eofl
    return (first:rest)


paragraph :: CharParser IndentState Element
paragraph = do
    notFollowedBy (string "  ")
    notFollowedBy (char '*')
    first <- text
    rest <- many $ try (curindent >> text)
    optional emptyline
    return $ Paragraph $ Sequence $ mergerawtexts $ addspaces (first:rest)
  where
  addspaces :: [[Text]] -> [Text]
  addspaces [] = []
  addspaces [x] = x
  addspaces (x:xs) = x ++ [RawText " "] ++ (addspaces xs)
  mergerawtexts :: [Text] -> [Text] -- mergerawtexts isn't strictly necessary, but makes for nicer output
  mergerawtexts [] = []
  mergerawtexts ((RawText r0):(RawText r1):xs) = mergerawtexts ((RawText (r0++r1)):xs)
  mergerawtexts ((RawText ""):xs) = mergerawtexts xs
  mergerawtexts (t:xs) = (t:mergerawtexts xs)

verbatimline = do
    curindent
    vtext <- manyTill anyChar eofl
    return (vtext ++ "\n")

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
    key <- between (char '[') (char ']') (many (noneOf "]\n"))
    skipMany (char ' ')
    url <- between (char '<') (char '>') (many (noneOf ">\n"))
    skipMany (char ' ')
    eofl
    eofl
    return $ LinkDef key url

{- We could left-factor the grammar below because there are a bunch of
 - backtracking steps for the initial list marker, the initial block
 - marker, ...
 -
 - This would be probably unmeasurably faster and more elegant in the
 - grammar sense but would cost us code elegance.
 -}
element :: CharParser IndentState Element
element = do
    curindent
    try linkdef <|> (header <?> "header")
                <|> (olist <?> "olist")
                <|> (ulist <?> "ulist")
                <|> (verbatim <?> "verbatim") -- verbatim before block
                <|> (block <?> "block") -- block before paragraph
                <|> (paragraph <?> "paragraph")


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
    removeModeline xs = unlines $ filter (/= modeline) $ lines xs
    modeline = "-*- mode: markup; -*-"

--
-- Entry Points
--

parseMarkup :: String -> Either ParseError Document
parseMarkup = parseMarkupWithOptions (ParseOptions True ["note"])

parseMarkupWithOptions :: ParseOptions -> String -> Either ParseError Document
parseMarkupWithOptions options input = runParser document (IndentState 0 False 0 options) "markup" $ preprocess input

