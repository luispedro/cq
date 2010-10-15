module Main where

import Parse (parseMarkup)
import System.Environment

showMarkup :: String -> String
showMarkup input = case parseMarkup input of
        Left err -> show err
        Right vals -> show vals

main :: IO ()
main = do
    args <- getArgs
    input <- readFile (args !! 0)
    putStrLn $ showMarkup input

