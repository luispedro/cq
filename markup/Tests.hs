module Main where

import Test.HUnit
import Text.ParserCombinators.Parsec
import Markup
import Parse
import Debug.Trace

parseJust :: CharParser IndentState tk -> String -> Either ParseError tk
parseJust parser input = (runParser parser (IndentState 0 False 0) "test" input)

tracex x = trace (show x) x

checkParsed (Left _) = False
checkParsed (Right _) = True

tests = TestList [indentline, indentline_empty, indentline_space, indentline_space2, t_emptyline, t_paragraph, t_text, text_br, text_br_inner, indentline_br_inner, many_indentline_br_inner, note, note_bad, indentline_br_inner_fail, many_indentline_br_inner_fail, many_indentline_br_inner_no_consume_all, many_indentline_br,indentline_br,many_indentline_br_inner_no_input, indentline_br_inner_no_input  ,h1,h2,h3,h3wcontent,h2wcontent, t_escapedchar, t_escapedchar_not,t_olistelem, t_olist, t_block2]
    where
    indentline_space = TestCase (assertBool "parse fails (match below)" $ not $ checkParsed pres)
        where
        pres = (parseJust text "  ")

    indentline_space2 = TestCase (assertBool "parse fails" $ not $ checkParsed pres)
        where
        pres = (parseJust text "  x")

    indentline_empty = TestCase (assertBool "parse fails (match below)" $ not $ checkParsed pres)
        where
        pres = (parseJust text "")

    t_emptyline = TestCase (assertBool "parse matches below" $ checkParsed pres)
        where
        pres = (parseJust emptyline "\n")

    indentline = TestCase (assertBool "parse matches below" $ checkParsed pres)
        where
        pres = (parseJust text "xxxas")

    t_paragraph = TestCase (assertBool "paragraph[ 1\\n2\\n... ]" $ checkParsed pres)
        where
        pres = (parseJust (paragraph >>eof) "one\ntwo\nthree\nfour\n")

    t_text = TestCase (assertBool "parse matches below" $ checkParsed pres)
        where
        pres = (parseJust text "one")

    text_br = TestCase (assertBool "parse matches below" $ checkParsed pres)
        where
        pres = (parseJust text "one}")

    text_br_inner = TestCase (assertBool "parse matches below" $ checkParsed pres)
        where
        pres = (runParser (text >> char '}' >> eof) (IndentState 0 False 1) "test" "one}")

    indentline_br_inner = TestCase (assertBool "parse matches below" $ checkParsed pres)
        where
        pres = (runParser (text >> char '}' >> eof) (IndentState 0 False 1) "test" "one}")

    indentline_br_inner_fail = TestCase (assertBool "NOT indentline[[ '}' ]]" $ not $ checkParsed pres)
        where
        pres = (runParser (text >> eof) (IndentState 0 False 1) "}" "one}")

    many_indentline_br_inner_fail = TestCase (assertBool "NOT many indentline[[ '}' ]]" $ not $ checkParsed pres)
        where
        pres = (runParser ((many text) >> eof) (IndentState 0 False 1) "}" "one}")

    many_indentline_br_inner_no_consume_all = TestCase (assertBool "NOT (many1 $ try text) >> eof [[ one} ]]" $ not $ checkParsed pres)
        where
        pres = (runParser ((many1 $ try text) >> eof) (IndentState 0 False 1) "test" "one}")

    many_indentline_br = TestCase (assertBool "(many1 $ try text) [[ one} ]]" $ checkParsed pres)
        where
        pres = (runParser ((many1 $ try text)) (IndentState 0 False 1) "test" "one}")

    indentline_br = TestCase (assertBool "text [[ one} ]]" $ checkParsed pres)
        where
        pres = (runParser text (IndentState 0 False 1) "test" "one}")

    many_indentline_br_inner = TestCase (assertBool "(many $ try text) >> (char '}') >> eof [[ one} ]]" $ checkParsed pres)
        where
        pres = (runParser ((many $ try text) >> char '}' >> eof) (IndentState 0 False 1) "test" "one}")

    many_indentline_br_inner_no_input = TestCase (assertBool "(many $try text) >> (char '}') >> eof [[ } ]]" $ checkParsed pres)
        where
        pres = (runParser ((many $ try text) >> char '}' >> eof) (IndentState 0 False 1) "test" "}")

    indentline_br_inner_no_input = TestCase (assertBool "NOT text >> (char '}') >> eof [[ } ]]" $ not $ checkParsed pres)
        where
        pres = (runParser (text >> char '}' >> eof) (IndentState 0 False 1) "test" "}")

    note = TestCase (assertBool "taggedtext[[ \\note{Test Me} ]]" (checkParsed pres))
        where
        pres = (runParser (taggedtext >> eof) (IndentState 0 False 0) "test" "\\note{Test Me}")

    note_bad = TestCase (assertBool "NOT taggedtext[[ \\note Test Me ]]" $ not $ checkParsed pres)
        where
        pres = (runParser (taggedtext >> eof) (IndentState 0 False 0) "test" "\\note Test Me")

    h1 = TestCase (assertBool "headermarker >> eof[[ *  ]]" $ checkParsed pres)
        where
        pres = (runParser (headermarker >> eof) (IndentState 0 False 0) "test" "* ")

    h2 = TestCase (assertBool "headermarker >> eof[[ **  ]]" $ checkParsed pres)
        where
        pres = (runParser (headermarker >> eof) (IndentState 0 False 0) "test" "** ")

    h3 = TestCase (assertBool "headermarker >> eof[[ ***  ]]" $ checkParsed pres)
        where
        pres = (runParser (headermarker >> eof) (IndentState 0 False 0) "test" "*** ")

    h2wcontent = TestCase (assertBool "header >> eof[[ ** My header ]]" $ checkParsed pres)
        where
        pres = (runParser (header >> eof) (IndentState 0 False 0) "test" "** My header")

    h3wcontent = TestCase (assertBool "header >> eof[[ *** My header ]]" $ checkParsed pres)
        where
        pres = (runParser (header >> eof) (IndentState 0 False 0) "test" "*** My header")

    t_escapedchar = TestCase (assertBool "escapedchar >> eof[ \\\\ ]]" $ checkParsed pres)
        where
        pres = (runParser (escapedchar >> eof) (IndentState 0 False 0) "test" "\\\\")

    t_escapedchar_not = TestCase (assertBool "NOT escapedchar >> eof[ \\other ]]"$ not $ checkParsed pres)
        where
        pres = (runParser (escapedchar >> eof) (IndentState 0 False 0) "test" "\\other")

    t_olistelem = TestCase (assertBool "olistelem >> eof[   # olist\\n ]]"$ checkParsed pres)
        where
        pres = (runParser (olistelem >> eof) (IndentState 0 False 0) "test" "  # olist\n")

    t_olist = TestCase (assertBool "olist >> eof[   # olist\\n  # olist2\\n ]]"$ checkParsed pres)
        where
        pres = (runParser (olist >> eof) (IndentState 0 False 0) "test" "  # olist\n  # olist2")
    t_block2 = TestCase (assertBool "block [ '  1\\n  2']" $ (==1) $ length p)
        where
        Right (Block p) = runParser (block >>= (\x -> (eof >> return x))) (IndentState 0 False 0) "test" "  oline1\n  oline2\n"

main = runTestTT tests

