module Parsec where


import Control.Applicative hiding (optional)
import Control.Monad
import Data.Functor


data ErrorMsg = EOF_ReachedError
              | ExpectedEOF_Error Char
              | ExpectedCharError Char Char
              | UnexpectedCharError Char


instance Show ErrorMsg where
    show EOF_ReachedError        = "Reached the end of input"
    show (ExpectedEOF_Error c)   = "Expected EOF, received " ++ show c
    show (ExpectedCharError e r) = "Expected " ++ show e ++ ", received " ++ show r
    show (UnexpectedCharError c) = "Received unexpected " ++ show c


type Pos = (Int, Int)


data ParseState = ParseState {
                    getParseString  :: String,
                    getParsePos     :: Pos
                  } deriving Show


updateParseState :: ParseState -> String -> ParseState
updateParseState state s = ParseState newS newPos
    where newPos = updateParsePos (getParsePos state) s
          newS   = drop (length s) (getParseString state)


updateParsePos :: Pos -> String -> Pos
updateParsePos = foldl update
    where update (linePos, charPos) c | c == '\n' = (linePos + 1, 0)
                                      | otherwise = (linePos, charPos + 1)


data Parser a = Parser { runParser :: ParseState -> Either ErrorMsg (a, ParseState) }


instance Functor Parser where
    f `fmap` pr = pr >>= return . f


instance Applicative Parser where
    pure = return
    f <*> pr = f >>= \g -> g `fmap` pr


instance Alternative Parser where
    empty = Parser $ \_ -> Left EOF_ReachedError

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
char ch = Parser $ \s -> case getParseString s of
                             (x: xs) -> if x == ch then Right (ch, updateParseState s [x])
                                                   else Left $ ExpectedCharError ch x
                             []      -> Left EOF_ReachedError


string :: String -> Parser String
string [] = return []
string (x: xs) = do
    y  <- char x
    y' <- string xs
    return (y: y')


notChar :: Char -> Parser Char
notChar ch = Parser $ \s -> case getParseString s of
                                (x: xs) -> if x /= ch then Right (x, updateParseState s [x])
                                                      else Left $ undefined
                                []      -> Left EOF_ReachedError


anyChar :: Parser Char
anyChar = Parser $ \s -> case getParseString s of
                            (x: xs) -> Right (x, updateParseState s [x])
                            []      -> Left EOF_ReachedError


emptyP :: Parser ()
emptyP = return ()


eof :: Parser ()
eof = Parser $ \s -> case getParseString s of
                        []        -> Right ((), s)
                        (x: _)    -> Left $ ExpectedEOF_Error x


ignoreP :: Parser a -> Parser ()
ignoreP pr = pr >> return () 


option :: a -> Parser a -> Parser a
option x pr = pr <|> (return x)


optional :: Parser a -> Parser ()
optional pr = (ignoreP pr) <|> emptyP


parse :: Parser a -> String -> Either ErrorMsg (a, ParseState)
parse pr s = do
    state <- return $ ParseState s (1, 1)
    runParser pr state
