module Main where

import Test.HUnit
import Text.ParserCombinators.Parsec (runParser, ParseError, CharParser)
import Markup
import Debug.Trace

parseJust :: CharParser IndentState tk -> String -> Either ParseError tk
parseJust parser input = (runParser parser (SimpleIndent 0 False) "test" input)

tracex x = trace (show x) x


tests = TestList [test_indentline, test_indentline_empty, test_indentline_space, test_indentline_space2, test_emptyline, test_paragraph]
    where
    test_indentline_space = TestCase (assertBool "parse fails (match below)" True)
        where
        Left err = (parseJust indentedline "  ")
    test_indentline_space2 = TestCase (assertBool "parse fails (match below)" True)
        where
        Left err = (parseJust indentedline "  x")
    test_indentline_empty = TestCase (assertBool "parse fails (match below)" True)
        where
        Left err = (parseJust indentedline "")
    test_emptyline = TestCase (assertBool "parse matches below)" True)
        where
        Right parsed = (parseJust emptyline "\n")
    test_indentline = TestCase (assertBool "parse matches below)" True)
        where
        Right parsed = (parseJust indentedline "xxxas")
    test_paragraph = TestCase (assertBool "parse matches below)" True)
        where
        Right parsed = (parseJust paragraph "one\ntwo\nthree\nfour\n\n  four\n  five\n")

main = runTestTT tests

