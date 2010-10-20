module Markup where
{- * Markup Structure
 -
 - The Markup structure shown below and maps directly to the definitions in
 - the spec.
 -
 - The block tags are hard-coded as a list to keep the parsing code simpler,
 - but you could change this and make it a parameter.
 -}
data Text = RawText String
    | LinkWKey String String -- link key
    | Link String -- link
    | InlineTag String [Text] -- tag str
    | BlockTag String [Element] -- tag str
    | Sequence [Text]

data Element = Paragraph Text
    | Verbatim String
    | UList [Element]
    | OList [Element]
    | UListElement [Element]
    | OListElement [Element]
    | Block [Element]
    | Header Integer String
    | LinkDef String String

data Document = Document [Element]

instance Show Text where
    show (RawText str) = (xmlEscape str)
    show (Link txt) = xmlShow "link" $ xmlEscape txt
    show (LinkWKey txt key) = xmlShow "link" $ concat [(xmlEscape txt), xmlShow "key" (xmlEscape key)]
    show (InlineTag tag elems) = xmlShow tag $ concat $ map show elems
    show (BlockTag tag elems) = xmlShow tag $ concat $ map show elems
    show (Sequence elems) = concat $ map show elems

instance Show Element where
    show (Paragraph txt) = xmlShow "p" (show txt)
    show (Verbatim str) = xmlShow "pre" (xmlEscape str)
    show (UList elems) = xmlShow "ul" (concat $ map show elems)
    show (UListElement elems) = xmlShow "li" (concat $map show elems)
    show (OList elems) = xmlShow "ol" (concat $map show elems)
    show (OListElement elems) = xmlShow "li" (concat $map show elems)
    show (Block elems) = xmlShow "blockquote" (concat $ map show elems)
    show (Header hl str) = xmlShow ("h" ++ (show hl)) (xmlEscape str)
    show (LinkDef link url) = xmlShow "link_def" $ concat [xmlShow "link" link, xmlShow "url" url]


instance Show Document where
    show (Document es) = xmlShow "body" (concat $ map show es)


xmlShow element content = concat $ ["<", element, ">", content, "</", element, ">"]
xmlEscape [] = []
xmlEscape ('<':cs) = "&lt;" ++ (xmlEscape cs)
xmlEscape ('>':cs) = "&gt;" ++ (xmlEscape cs)
xmlEscape ('&':cs) = "&amp;" ++ (xmlEscape cs)
xmlEscape (c:cs) = (c:xmlEscape cs)


