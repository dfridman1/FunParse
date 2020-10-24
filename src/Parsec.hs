module Parsec where


import Control.Applicative hiding (optional)
import Control.Monad
import Data.Functor


data ErrorMsg = EOF
              | MatchFail deriving Show


data Parser a = Parser { runParser :: String -> Either ErrorMsg (a, String) }


instance Functor Parser where
    f `fmap` pr = pr >>= return . f


instance Applicative Parser where
    pure = return
    f <*> pr = f >>= \g -> g `fmap` pr


instance Alternative Parser where
    empty = Parser $ \_ -> Left EOF

    lPr <|> rPr = Parser $ \s -> case (runParser lPr s, runParser rPr s) of
                                    (t@(Right _), _) -> t 
                                    (_, t@(Right _)) -> t
                                    (t, _)          -> t


oneOf :: (Foldable t, Alternative f) => t (f a) -> f a
oneOf = foldl (<|>) empty 


instance Monad Parser where
    return x = Parser $ \s -> Right (x, s)
    
    pr >>= f = Parser $ \s -> case runParser pr s of
                                Left err      -> Left err
                                Right (r, s') -> runParser (f r) s'


char :: Char -> Parser Char
char ch = Parser $ \s -> case s of
                             (x: xs) -> if x == ch then Right (ch, xs) else Left MatchFail
                             []      -> Left EOF


string :: String -> Parser String
string [] = return []
string (x: xs) = do
    y  <- char x
    y' <- string xs
    return (y: y')


notChar :: Char -> Parser Char
notChar ch = Parser $ \s -> case s of
                                (x: xs) -> if x /= ch then Right (x, xs) else Left MatchFail
                                []      -> Left EOF 


anyChar :: Parser Char
anyChar = Parser $ \s -> case s of
                            (x: xs) -> Right (x, xs)
                            []      -> Left EOF


emptyP :: Parser ()
emptyP = return ()


eof :: Parser ()
eof = Parser $ \s -> case s of
                        []        -> Right ((), [])
                        otherwise -> Left MatchFail


ignoreP :: Parser a -> Parser ()
ignoreP pr = pr >> return () 


option :: a -> Parser a -> Parser a
option x pr = pr <|> (return x)


optional :: Parser a -> Parser ()
optional pr = (ignoreP pr) <|> emptyP
