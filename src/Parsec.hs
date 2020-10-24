module Parsec where


import Control.Applicative hiding (optional, many)
import Control.Monad
import Data.Functor


type Pos = (Int, Int)


showPos :: Pos -> String
showPos (line, column) = "(Line " ++ show line ++ ", position " ++ show column ++ "):\n"


data ErrorMsg = EOF_ReachedError Pos
              | ExpectedEOF_Error Pos Char
              | ExpectedCharError Pos Char Char
              | UnexpectedCharError Pos Char


instance Show ErrorMsg where
    show (EOF_ReachedError p)      = showPos p ++ "reached the end of input"
    show (ExpectedEOF_Error p c)   = showPos p ++ "expected EOF, received " ++ show c
    show (ExpectedCharError p e r) = showPos p ++ "expected " ++ show e ++ ", received " ++ show r
    show (UnexpectedCharError p c) = showPos p ++ "received unexpected " ++ show c


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
    where update (line, column) c | c == '\n' = (line + 1, 0)
                                      | otherwise = (line, column + 1)


data Parser a = Parser { runParser :: ParseState -> Either ErrorMsg (a, ParseState) }


instance Functor Parser where
    f `fmap` pr = pr >>= return . f


instance Applicative Parser where
    pure = return
    f <*> pr = f >>= \g -> g `fmap` pr


instance Alternative Parser where
    empty = Parser $ \s -> Left $ EOF_ReachedError (getParsePos s)

    lPr <|> rPr = Parser $ \s -> case (runParser lPr s, runParser rPr s) of
                                    (t@(Right _), _) -> t 
                                    (_, t@(Right _)) -> t
                                    (_, t)          -> t


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
                                                   else Left $ ExpectedCharError (getParsePos s) ch x
                             []      -> Left $ EOF_ReachedError (getParsePos s)


string :: String -> Parser String
string = sequence . map char


notChar :: Char -> Parser Char
notChar ch = Parser $ \s -> case getParseString s of
                                (x: xs) -> if x /= ch then Right (x, updateParseState s [x])
                                                      else Left $ undefined
                                []      -> Left $ EOF_ReachedError (getParsePos s)


digit :: Parser Char
digit = oneOf $ map (char . head . show) [0..9]


digits :: Parser String
digits = many1 digit


anyChar :: Parser Char
anyChar = Parser $ \s -> case getParseString s of
                            (x: xs) -> Right (x, updateParseState s [x])
                            []      -> Left $ EOF_ReachedError (getParsePos s)


emptyP :: Parser ()
emptyP = return ()


eof :: Parser ()
eof = Parser $ \s -> case getParseString s of
                        []        -> Right ((), s)
                        (x: _)    -> Left $ ExpectedEOF_Error (getParsePos s) x


ignoreP :: Parser a -> Parser ()
ignoreP pr = pr >> return () 


option :: a -> Parser a -> Parser a
option x pr = pr <|> (return x)


optional :: Parser a -> Parser ()
optional pr = (ignoreP pr) <|> emptyP


many :: Parser a -> Parser [a]
many pr = Parser $ \s -> case runParser pr s of
                            Left err      -> Right ([], s)
                            Right (r, s') -> case runParser (many pr) s' of
                                                Right ([], s'') -> Right ([r], s')
                                                Right (r', s'') -> Right (r: r', s'')


many1 :: Parser a -> Parser [a]
many1 pr = do
    x  <- pr
    xs <- many pr
    return $ x: xs


parse :: Parser a -> String -> Either ErrorMsg (a, ParseState)
parse pr s = do
    state <- return $ ParseState s (1, 1)
    runParser pr state
