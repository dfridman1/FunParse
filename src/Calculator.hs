module Calculator where


import Data.List.Split ( splitOn )
import Control.Applicative ( Alternative((<|>)) )
import FunParse ( Parser(), char, digits, parse, many1 )


data Expr = Sum [Expr]
          | Prod [Expr]
          | Lit Double
          | Neg Expr
          | Inv Expr
            deriving Show


eval :: Expr -> Double
eval (Sum x) = binop (+) x
eval (Prod x) = binop (*) x
eval (Lit x)  = x
eval (Neg x)  = negate $ eval x
eval (Inv x) = 1 / eval x


binop :: (Double -> Double -> Double) -> [Expr] -> Double
binop op = foldl1 op . map eval


expr :: Parser Expr
expr = add <|> mul <|> term


add :: Parser Expr
add = do
    x  <- factor
    xs <- many1 summandP
    return $ Sum (x: xs)
    where summandP = do
            op <- char '-' <|> char '+'
            fc <- factor
            let f = if op == '-' then Neg else id
            return $ f fc


factor :: Parser Expr
factor = mul <|> term


mul :: Parser Expr
mul = do
    x  <- term
    xs <- many1 $ factorP
    return $ Prod (x: xs)
    where factorP = do
            op <- char '/' <|> char '*'
            te <- term
            let f = if op == '/' then Inv else id
            return $ f te


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
                                Right (x, _) -> if eval x ~= (read r :: Double)
                                                then Right "Success!"
                                                else Left e
    where x ~= y = abs (x - y) < eps
          eps    = 0.01


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
