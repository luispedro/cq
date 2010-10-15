module Main where

import Markup
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    input <- readFile (args !! 0)
    putStrLn $ parseMarkup input

