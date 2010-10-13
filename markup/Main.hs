module Main where

import Markup
import System.Environment

main :: IO ()
main = do
    input <- readFile "input.mup"
    print $ parseMarkup input

