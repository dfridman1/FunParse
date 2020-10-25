module Main where

import Parsec


flatten :: [[a]] -> [a]
flatten = foldr join []
    where join xs acc = foldr (:) acc xs


nestedParens :: Parser String
nestedParens = do
    result <- flatten `fmap` many f
    eof
    return result
        where f = do
                open  <- char '('
                body  <- flatten `fmap` many f
                close <- char ')'
                return $ open: (body ++ [close])

main :: IO ()
main = do
    putStrLn "Enter expression:"
    line <- getLine
    let result = parse nestedParens line
    putStrLn $ show result
    main
