module JsonParser where


import Control.Applicative ( Alternative((<|>)) )
import Data.List ( intercalate )
import Parsec ( Parser(), digits, char, sepBy, symbol, parse, notChar, many, choice, skipMany )


data JValue = JNull
            | JNumber Int
            | JObject [(String, JValue)]
            | JArray [JValue]
            | JString String
            deriving Show


compressJValue :: JValue -> String
compressJValue JNull = show "null"
compressJValue (JNumber n) = show n
compressJValue (JString s) = show s
compressJValue (JArray arr) = "[" ++ values ++ "]"
    where values = "," `intercalate` map compressJValue arr
compressJValue (JObject o) = "{" ++ values ++ "}"
    where values = "," `intercalate` map compressJValuePair o
          compressJValuePair (key, value) = show key ++ ":" ++ compressJValue value


jvalue :: Parser JValue
jvalue = choice parsers
    where parsers = [jobject, jarray, jnumber, jstring, jnull]


jnumber ::Parser JValue
jnumber = fmap (JNumber . toInt) digits
    where toInt x = read x :: Int


jarray :: Parser JValue
jarray = do
    makeSeparator '['
    xs <- jvalue `sepBy` makeSeparator ','
    makeSeparator ']'
    return $ JArray xs


jobject :: Parser JValue
jobject = do
    makeSeparator '{'
    pairs <- parsePair `sepBy` makeSeparator ','
    makeSeparator '}'
    return $ JObject pairs
        where parsePair = do
                key <- string
                makeSeparator ':'
                value <- jvalue
                return (key, value)


jnull :: Parser JValue
jnull = symbol "null" >> return JNull


jstring :: Parser JValue
jstring = fmap JString string


string :: Parser String
string = do
    char '"'
    xs <- many $ symbol "\\\"" <|> (fmap return $ notChar '"')
    char '"'
    return $ flatten xs


flatten :: [[a]] -> [a]
flatten = foldr join []
    where join xs acc = foldr (:) acc xs


space :: Parser ()
space = skipMany $ choice $ map char [' ', '\t', '\n']


makeSeparator :: Char -> Parser ()
makeSeparator ch = space >> char ch >> space


json :: String -> String
json = either show (show . fst) . parse jvalue


compressJsonFile:: FilePath -> FilePath -> IO ()
compressJsonFile inp out = do
    content <- readFile inp
    let value = either show (compressJValue . fst) $ parse jvalue content
    writeFile out value
