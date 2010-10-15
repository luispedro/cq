module Markup where

import XML

data Text = RawText String
    | LinkText String String -- link key
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

data Document = Document [Element]

blockTags = ["note"]

{- * Markup Structure
 -
 - The Markup structure is shown above and maps directly to the definitions in
 - the spec.
 -
 - The block tags are hard-coded as a list to keep the parsing code simpler,
 - but you could change this and make it a parameter.
 -}

instance Show Text where
    show (RawText str) = str
    show (InlineTag tag elems) = xmlShow 0 tag $ concat $ map show elems
    show (BlockTag tag elems) = xmlShow 0 tag $ concat $ map show elems
    show (Sequence elems) = concat $ map show elems

instance Show Element where
    show = show' 0
        where
        show' n (Paragraph txt) = xmlShow n "p" (show txt)
        show' n (Verbatim str) = xmlShow n "pre" str
        show' n (UList elems) = xmlShow n "ul" (concat $ map (show' (n+1)) elems)
        show' n (UListElement elems) = xmlShow n "li" (concat $map (show' (n+1)) elems)
        show' n (OList elems) = xmlShow n "ol" (concat $map (show' (n+1)) elems)
        show' n (OListElement elems) = xmlShow n "li" (concat $map (show' (n+1)) elems)
        show' n (Block elems) = xmlShow n "blockquote" (concat $ map (show' (n+1)) elems)
        show' n (Header hl str) = xmlShow n ("h" ++ (show hl)) str


instance Show Document where
    show (Document es) = xmlShow 0 "body" (concat $ map show es)

