module Calculator where


import Data.List.Split ( splitOn )
import Control.Applicative ( Alternative((<|>)) )
import Parsec ( Parser(), char, digits, parse )


data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Lit Double
            deriving Show


eval :: Expr -> Double
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x / eval y
eval (Lit x)   = x


expr :: Parser Expr
expr = add <|> mul <|> term


add :: Parser Expr
add = do
    t <- factor
    char '+'
    f <- expr
    return $ Add t f


factor :: Parser Expr
factor = mul <|> term


mul :: Parser Expr
mul = do
    e1 <- term
    char '*'
    e2 <- factor
    return $ Mul e1 e2


term :: Parser Expr
term = lit <|> paren


lit :: Parser Expr
lit = digits >>= return . Lit . read


paren :: Parser Expr
paren = do
    char '('
    e <- expr
    char ')'
    return e


test :: FilePath -> IO ()
test fp = do
    examples <- lines `fmap` readFile fp
    sequence_ $ testExampleIO `map` examples
    putStrLn "Tests finished."


testExampleIO :: String -> IO ()
testExampleIO = either putStrLn (\_ -> return ()) . testExample


testExample :: String -> Either String String
testExample s = case "\t" `splitOn` s of
                    [e, r] -> case parse expr e of
                                Left   _     -> Left "Parse error!"
                                Right (x, _) -> if eval x == (read r :: Double)
                                                then Right "Success!"
                                                else Left e


mainTest :: IO ()
mainTest = test "data/test/arithm_test_cases.txt"


main = do
    putStr "Enter expression: "
    content <- getLine
    case parse expr content of
        Left err -> putStrLn $ show err
        Right (r, _) -> putStrLn $ show r ++ " = " ++ (showR $ eval r)
    main
    where showR x = case doubleToMaybeInt x of
                        Just n -> show n
                        _      -> show x
          doubleToMaybeInt x | floor x == ceiling x = Just $ round x
                             | otherwise            = Nothing
