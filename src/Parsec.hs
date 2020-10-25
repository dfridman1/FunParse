module Parsec (
        Parser(),
        char,
        symbol,
        eof,
        option,
        optional,
        parse,
        notChar,
        many,
        many1,
        digit,
        digits,
        anyChar,
        skipMany,
        sepBy,
        sepBy1,
        space
) where


import Control.Applicative ( Alternative((<|>), empty) )
import Control.Monad ( void )


type Pos = (Int, Int)


showPos :: Pos -> String
showPos (line, column) = "(Line " ++ show line ++ ", position " ++ show column ++ "):\n"


data ErrorMsg = EOFReachedError Pos
              | ExpectedEOFError Pos Char
              | ExpectedCharError Pos Char Char
              | UnexpectedCharError Pos Char


instance Show ErrorMsg where
    show (EOFReachedError p)       = showPos p ++ "reached EOF"
    show (ExpectedEOFError p c)    = showPos p ++ "expected EOF, received " ++ show c
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


newtype Parser a = Parser { runParser :: ParseState -> Either ErrorMsg (a, ParseState) }


instance Functor Parser where
    f `fmap` pr = pr >>= return . f


instance Applicative Parser where
    pure = return
    f <*> pr = f >>= \g -> g `fmap` pr


instance Alternative Parser where
    empty = Parser $ \s -> Left $ EOFReachedError (getParsePos s)

    lPr <|> rPr = Parser $ \s -> case (runParser lPr s, runParser rPr s) of
                                    (t@(Right _), _) -> t
                                    (_, t@(Right _)) -> t
                                    (_, t)          -> t



instance Monad Parser where
    return x = Parser $ \s -> Right (x, s)

    pr >>= f = Parser $ \s -> case runParser pr s of
                                Left err      -> Left err
                                Right (r, s') -> runParser (f r) s'


char :: Char -> Parser Char
char ch = Parser $ \s -> case getParseString s of
                             (x: _)  -> if x == ch then Right (ch, updateParseState s [x])
                                                   else Left $ ExpectedCharError (getParsePos s) ch x
                             []      -> Left $ EOFReachedError (getParsePos s)


space :: Parser Char
space = char ' '


symbol :: String -> Parser String
symbol = mapM char


notChar :: Char -> Parser Char
notChar ch = Parser $ \s -> case getParseString s of
                                (x: _)  -> if x /= ch then Right (x, updateParseState s [x])
                                                      else Left $ UnexpectedCharError (getParsePos s) x
                                []      -> Left $ EOFReachedError (getParsePos s)


oneOf :: (Foldable t, Alternative f) => t (f a) -> f a
oneOf = foldl (<|>) empty


digit :: Parser Char
digit = oneOf $ map (char . head . show) [0..9]


digits :: Parser String
digits = many1 digit


anyChar :: Parser Char
anyChar = Parser $ \s -> case getParseString s of
                            (x: _)  -> Right (x, updateParseState s [x])
                            []      -> Left $ EOFReachedError (getParsePos s)


emptyP :: Parser ()
emptyP = return ()


eof :: Parser ()
eof = Parser $ \s -> case getParseString s of
                        []        -> Right ((), s)
                        (x: _)    -> Left $ ExpectedEOFError (getParsePos s) x


ignoreP :: Parser a -> Parser ()
ignoreP pr = void pr


option :: a -> Parser a -> Parser a
option x pr = pr <|> return x


optional :: Parser a -> Parser ()
optional pr = ignoreP pr <|> emptyP


many :: Parser a -> Parser [a]
many pr = Parser $ \s -> case runParser pr s of
                            Left _        -> Right ([], s)
                            Right (r, s') -> case runParser (many pr) s' of
                                                Right ([], _)   -> Right ([r], s')
                                                Right (r', s'') -> Right (r: r', s'')


many1 :: Parser a -> Parser [a]
many1 pr = do
    x  <- pr
    xs <- many pr
    return $ x: xs


sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 pr sep = do
    x <- pr
    xs <- many f
    return $ x: xs
        where f = do
                sep
                pr

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy pr = option [] . sepBy1 pr


skipMany :: Parser a -> Parser ()
skipMany = void . many


parse' :: Parser a -> String -> Either ErrorMsg (a, ParseState)
parse' pr s = do
    let state = ParseState s (1, 1)
    runParser pr state


parse :: Show a => Parser a -> String -> String
parse pr = either show show . parse' pr
