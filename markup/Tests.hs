module Main where

import Test.HUnit
import Text.ParserCombinators.Parsec (parse, Parser, ParseError)
import Markup

parseJust :: Parser tk -> String -> Either ParseError tk
parseJust parser input = (parse parser "test" input)


testLineIndent0 = TestCase (assertEqual "should be zero" 0 level)
    where
    Right (IndentedLine level _) = (parseJust line "this is a unindented line\n")

testLineIndent2 = TestCase (assertEqual "should be 2" 2 level)
    where
    Right (IndentedLine level _) = (parseJust line "  this is a 2-indented line\n")

tests = TestList [TestLabel "test0" testLineIndent0, TestLabel "test2" testLineIndent2]

main = runTestTT tests

