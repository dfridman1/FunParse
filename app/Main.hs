module Main where


import Control.Monad ( void )
import Parsec ( Parser(), char, digits, eof, parse, sepBy, skipMany, space )


flatten :: [[a]] -> [a]
flatten = foldr join []
    where join xs acc = foldr (:) acc xs



parseList :: Parser [Int]
parseList = do
    skipMany space
    char '['
    skipMany space
    xs <- digits `sepBy` sep
    skipMany space
    char ']'
    skipMany space
    eof
    return $ readInt `map` xs
        where readInt x = read x :: Int
              sep       = sequence_ [skipMany space, void $ char ',', skipMany space]

main :: IO ()
main = do
    putStrLn "Enter expression:"
    line <- getLine
    return ()
    let result = parse parseList line
    putStrLn result
    main
