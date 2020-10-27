module Main where


import System.Environment ( getArgs )
import JsonParser ( compressJsonFile )


main :: IO ()
main = do
    args <- getArgs
    case args of
        [inp, out] -> compressJsonFile inp out
        _          -> putStrLn "Expecting exactly 2 arguments."
