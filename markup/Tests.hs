module Main where

import Test.HUnit
import Text.ParserCombinators.Parsec (parse, Parser, ParseError)
import Markup
import Debug.Trace

parseJust :: Parser tk -> String -> Either ParseError tk
parseJust parser input = (parse parser "test" input)

tracex x = trace (show x) x


tests = TestList [testLineIndent0, testLineIndent2, testLineIndent_tab, testLineIndent_tab_neol, test_isBlankLine0, test_isBlankLine1, test_ParagraphEmpty, test_Paragraph, test_ParagraphEOF, test_ParagraphNLEOF]
    where
    testLineIndent0 = TestCase (assertEqual "should be zero" 0 level)
        where
        Right (IndentedLine level _ _) = (parseJust line "this is a unindented line\n")

    testLineIndent2 = TestCase (assertEqual "should be 2" 2 level)
        where
        Right (IndentedLine level _ _) = parseJust line "  this is a two-indented line\n"

    testLineIndent_tab = TestCase (assertEqual "should be 8" 8 level)
        where
        Right (IndentedLine level _ _) = parseJust line "\tthis is a tab-indented line with special chars @#%.;<[()]>\n"

    testLineIndent_tab_neol = TestCase (assertEqual "should be 9" 9 level)
        where
        Right (IndentedLine level _ _) = parseJust line "\t his is a tab indented line with no eol"

    test_isBlankLine0 = TestCase (assertBool "should be blank" (isBlankLine parsed))
        where
        Right parsed = parseJust line "     "
    test_isBlankLine1 = TestCase (assertBool "should NOT be blank" (not $ isBlankLine parsed))
        where
        Right parsed = parseJust line "     something"
    test_ParagraphEmpty = TestCase (assertBool "check for match below" True)
        where
        Left err = parseJust paragraph ""
    test_Paragraph = TestCase (assertBool "check for match below" True)
        where
        Right _ = parseJust paragraph "This is a paragraph\nWith two lines\n\n"
    test_ParagraphNLEOF = TestCase (assertBool "check for match below" True)
        where
        Right _ = parseJust paragraph "This is a paragraph\nWith two lines and no empty line at end\n"
    test_ParagraphEOF = TestCase (assertBool "check for match below" True)
        where
        Right _ = parseJust paragraph "This is a paragraph\nWith two lines and the last does not finish"

main = runTestTT tests

