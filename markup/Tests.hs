module Main where

import Test.HUnit
import Text.ParserCombinators.Parsec
import Markup
import Parse
import Debug.Trace

parseJust :: CharParser IndentState tk -> String -> Either ParseError tk
parseJust parser input = (runParser parser (SimpleIndent 0 False 0) "test" input)

tracex x = trace (show x) x

checkParsed (Left _) = False
checkParsed (Right _) = True

tests = TestList [indentline, indentline_empty, indentline_space, indentline_space2, t_emptyline, t_paragraph, t_text, text_br, text_br_inner, indentline_br_inner, many_indentline_br_inner, note, note_bad, indentline_br_inner_fail, many_indentline_br_inner_fail, many_indentline_br_inner_no_consume_all, many_indentline_br,indentline_br,many_indentline_br_inner_no_input, indentline_br_inner_no_input  ,h1,h2,h3,h3wcontent,h2wcontent]
    where
    indentline_space = TestCase (assertBool "parse fails (match below)" $ not $ checkParsed pres)
        where
        pres = (parseJust indentedline "  ")

    indentline_space2 = TestCase (assertBool "parse fails" $ not $ checkParsed pres)
        where
        pres = (parseJust indentedline "  x")

    indentline_empty = TestCase (assertBool "parse fails (match below)" $ not $ checkParsed pres)
        where
        pres = (parseJust indentedline "")

    t_emptyline = TestCase (assertBool "parse matches below" $ checkParsed pres)
        where
        pres = (parseJust emptyline "\n")

    indentline = TestCase (assertBool "parse matches below" $ checkParsed pres)
        where
        pres = (parseJust indentedline "xxxas")

    t_paragraph = TestCase (assertBool "parse matches below" $ checkParsed pres)
        where
        pres = (parseJust paragraph "one\ntwo\nthree\nfour\n\n  four\n  five\n")

    t_text = TestCase (assertBool "parse matches below" $ checkParsed pres)
        where
        pres = (parseJust text "one")

    text_br = TestCase (assertBool "parse matches below" $ checkParsed pres)
        where
        pres = (parseJust text "one}")

    text_br_inner = TestCase (assertBool "parse matches below" $ checkParsed pres)
        where
        pres = (runParser (text >> char '}' >> eof) (SimpleIndent 0 False 1) "test" "one}")

    indentline_br_inner = TestCase (assertBool "parse matches below" $ checkParsed pres)
        where
        pres = (runParser (indentedline >> char '}' >> eof) (SimpleIndent 0 False 1) "test" "one}")

    indentline_br_inner_fail = TestCase (assertBool "NOT indentline[[ '}' ]]" $ not $ checkParsed pres)
        where
        pres = (runParser (indentedline >> eof) (SimpleIndent 0 False 1) "}" "one}")

    many_indentline_br_inner_fail = TestCase (assertBool "NOT many indentline[[ '}' ]]" $ not $ checkParsed pres)
        where
        pres = (runParser ((many indentedline) >> eof) (SimpleIndent 0 False 1) "}" "one}")

    many_indentline_br_inner_no_consume_all = TestCase (assertBool "NOT (many1 $ try indentedline) >> eof [[ one} ]]" $ not $ checkParsed pres)
        where
        pres = (runParser ((many1 $ try indentedline) >> eof) (SimpleIndent 0 False 1) "test" "one}")

    many_indentline_br = TestCase (assertBool "(many1 $ try indentedline) [[ one} ]]" $ checkParsed pres)
        where
        pres = (runParser ((many1 $ try indentedline)) (SimpleIndent 0 False 1) "test" "one}")

    indentline_br = TestCase (assertBool "indentedline [[ one} ]]" $ checkParsed pres)
        where
        pres = (runParser indentedline (SimpleIndent 0 False 1) "test" "one}")

    many_indentline_br_inner = TestCase (assertBool "(many $ try indentedline) >> (char '}') >> eof [[ one} ]]" $ checkParsed pres)
        where
        pres = (runParser ((many $ try indentedline) >> char '}' >> eof) (SimpleIndent 0 False 1) "test" "one}")

    many_indentline_br_inner_no_input = TestCase (assertBool "(many $try indentedline) >> (char '}') >> eof [[ } ]]" $ checkParsed pres)
        where
        pres = (runParser ((many $ try indentedline) >> char '}' >> eof) (SimpleIndent 0 False 1) "test" "}")

    indentline_br_inner_no_input = TestCase (assertBool "NOT indentedline >> (char '}') >> eof [[ } ]]" $ not $ checkParsed pres)
        where
        pres = (runParser (indentedline >> char '}' >> eof) (SimpleIndent 0 False 1) "test" "}")

    note = TestCase (assertBool "taggedtext[[ \\note{Test Me} ]]" (checkParsed pres))
        where
        pres = (runParser (taggedtext >> eof) (SimpleIndent 0 False 0) "test" "\\note{Test Me}")

    note_bad = TestCase (assertBool "NOT taggedtext[[ \\note Test Me ]]" $ not $ checkParsed pres)
        where
        pres = (runParser (taggedtext >> eof) (SimpleIndent 0 False 0) "test" "\\note Test Me")

    h1 = TestCase (assertBool "headermarker >> eof[[ *  ]]" $ checkParsed pres)
        where
        pres = (runParser (headermarker >> eof) (SimpleIndent 0 False 0) "test" "* ")

    h2 = TestCase (assertBool "headermarker >> eof[[ **  ]]" $ checkParsed pres)
        where
        pres = (runParser (headermarker >> eof) (SimpleIndent 0 False 0) "test" "** ")

    h3 = TestCase (assertBool "headermarker >> eof[[ ***  ]]" $ checkParsed pres)
        where
        pres = (runParser (headermarker >> eof) (SimpleIndent 0 False 0) "test" "*** ")

    h2wcontent = TestCase (assertBool "header >> eof[[ ** My header ]]" $ checkParsed pres)
        where
        pres = (runParser (header >> eof) (SimpleIndent 0 False 0) "test" "** My header")

    h3wcontent = TestCase (assertBool "header >> eof[[ *** My header ]]" $ checkParsed pres)
        where
        pres = (runParser (header >> eof) (SimpleIndent 0 False 0) "test" "*** My header")

main = runTestTT tests

