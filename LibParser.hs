{-# LANGUAGE LambdaCase #-}
module LibParser where

import Data.Maybe
import Data.List


data Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

data Value =  GInt Int
            | GFloat Float
            | GString String
            | GSymbol String
            | GBool Bool
            deriving (Eq, Show)

-- Basic blocks

parseChar :: Char -> Parser Char
parseChar c = Parser (\case
                        [] -> Nothing
                        (x:xs) -> if x == c
                            then Just (x, xs)
                            else Nothing)

parseAnyChar :: String -> Parser Char
parseAnyChar c = Parser (\case
                            [] -> Nothing
                            (x:xs) -> if x `elem` c
                                    then Just (x, xs)
                                    else Nothing)

parseDigit :: Parser Char
parseDigit = parseAnyChar ['0'..'9']

parseDigits :: Parser [Char]
parseDigits = parseSome parseDigit

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 = Parser (\case
                            [] -> Nothing
                            s -> if isJust (runParser p1 s)
                                then runParser p1 s
                                else runParser p2 s)

parseThisString :: String -> Parser String
parseThisString s = Parser (\case
                                [] -> Nothing
                                s' -> if s `isPrefixOf` s'
                                    then Just (s, drop (length s) s')
                                    else Nothing)

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 = Parser pAnd
    where
        pAnd s
            | isJust p1' = case runParser p2 $ snd $ fromJust p1' of
                       Just (a, b) -> Just ((fst $ fromJust p1', a), b)
                       Nothing -> Nothing
            | otherwise = Nothing
            where
                p1' = runParser p1 s

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 = Parser pAndWith
    where
        pAndWith s = case runParser (parseAnd p1 p2) s of
                           Just ((a, b), s) -> Just (f a b, s)
                           Nothing -> Nothing

-- zero or more elements to be parsed
parseMany :: Parser a -> Parser [a]
parseMany p = Parser pMany
    where
        pMany s = case runParser p s of
                Nothing -> Just ([], s)
                Just (x, rest1) -> case runParser (parseMany p) rest1 of
                                    Just (y, rest2) -> Just (x:y, rest2)
                                    Nothing -> Just ([x], rest1)

-- at least one element to be parsed
parseSome :: Parser a -> Parser [a]
parseSome p = Parser pSome
    where
        pSome s = case runParser (parseMany p) s of
                Just ([], str) -> Nothing
                x -> x

-- More specific

parseUInt :: Parser Int
parseUInt = Parser pUInt
    where
        pUInt s = case runParser parseDigits s of
                       Just (int, rest) -> Just (read int :: Int, rest)
                       Nothing -> Nothing

parseNegInt :: Parser Int
parseNegInt = Parser pNegInt
    where
        pNegInt s = case runParser (parseAnd (parseChar '-') parseUInt) s of
                       Just (('-', nb), rest) -> Just (-nb, rest)
                       Just _ -> Nothing
                       Nothing -> Nothing

parseInt :: Parser Int
parseInt = parseUInt `parseOr` parseNegInt

parseFloat :: Parser Float
parseFloat = Parser pFloat
    where
        pFloat s = case runParser (parseAnd (parseDigits `parseAnd` parseChar '.') parseDigits) s of
                    Just (((nb, _), after), rest) -> Just (read (nb ++ "." ++ after) :: Float, rest)
                    Nothing -> Nothing

parseNumber :: Parser (Either Float Int)
parseNumber = Parser pNumber
    where
        pNumber s = case runParser parseFloat s of
                        Just (nb, rest) -> Just (Left nb, rest)
                        Nothing -> case runParser parseInt s of
                                    Just (nb, rest) -> Just (Right nb, rest)
                                    Nothing -> Nothing

-- -- GLaDOS parser

parseOpenParen :: Parser Char
parseOpenParen = parseChar '('

parseCloseParen :: Parser Char
parseCloseParen = parseChar ')'

parseComa :: Parser Char
parseComa = parseChar ','

parseAllSpaces :: Parser String
parseAllSpaces = parseSome (parseAnyChar " \t\n")

parseSomeSpaces :: Parser String
parseSomeSpaces = parseSome (parseChar ' ')

parseManySpaces :: Parser String
parseManySpaces = parseMany (parseChar ' ')

parseSymbol :: Parser String
parseSymbol = parseSome (parseAnyChar $ ['a'..'z'] ++ ['A'..'Z'] ++ "'" ++ "#")

parseTrue :: Parser Bool
parseTrue = Parser pTrue
    where
        pTrue s = case runParser (parseThisString "#t") s of
                    Just (_, rest) -> Just (True, rest)
                    Nothing -> Nothing

parseFalse :: Parser Bool
parseFalse = Parser pFalse
    where
        pFalse s = case runParser (parseThisString "#f") s of
                    Just (_, rest) -> Just (False, rest)
                    Nothing -> Nothing

parseBool :: Parser Bool
parseBool = parseTrue `parseOr` parseFalse

parseValue :: Parser Value
-- parseValue = parseBool `parseOr` parseFloat
parseValue = Parser pValue
    where
        pValue s = case runParser parseBool s of
                        Just (val, rest) -> Just (GBool val, rest)
                        Nothing -> case runParser parseFloat s of
                            Just (val, rest) -> Just (GFloat val, rest)
                            Nothing -> case runParser parseInt s of
                                Just (val, rest) -> Just (GInt val, rest)
                                Nothing -> case runParser parseSymbol s of
                                    Just (val, rest) -> Just (GSymbol val, rest)
                                    Nothing -> Nothing

parsePair :: Parser a -> Parser (a, a)
parsePair p = parseAndWith seq parseOpenParen
                (parseAndWith (,) p
                (parseAndWith seq parseSomeSpaces
                (parseAndWith const p
                                    parseCloseParen)))

-- parseList :: Parser a -> Parser [a]
-- parseList p = parseEmptyList `parseOr` parseAndWith seq parseOpenParen
--                                         (parseAndWith (:) p
--                                         (parseAndWith const (parseListNum p) parseCloseParen))

-- parseEmptyList :: Parser [a]
-- parseEmptyList = parseAndWith (\ x y -> []) parseOpenParen parseCloseParen

-- parseListNum :: Parser a -> Parser [a]
-- parseListNum p = Parser pListNum
--     where
--         pListNum s = case runParser parseSomeSpaces s of
--                         Just (_, rest) -> runParser (parseAndWith (:) p (parseListNum p)) rest
--                         Nothing -> Just ([], s)
