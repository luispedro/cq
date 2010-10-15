module XML where

concatenate :: [String] -> String
concatenate = foldr1 (++)


xmlShow level element content = concatenate $ [doindent level, "<", element, ">\n", doindent level, content, "\n", doindent level, "</", element, ">\n"]
    where
        doindent 0 = ""
        doindent n = "  " ++ doindent (n-1)

