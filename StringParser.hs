{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
module StringParser where

import Text.Parsec
import Text.Parsec.Char
import Data.List(nub, (\\))
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

data EscapeType c = WordAlpha | AntiWordAlpha | JustEscape c
    deriving (Show, Eq)

lexer = P.makeTokenParser haskellDef
integer = P.integer lexer
braces = P.braces lexer
brackets = P.brackets lexer
minusSingal = char '-'
savedChars = "\\[]~{}"
intToCh :: Int -> Char
intToCh = toEnum
wordAlpha = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
antiWordAlpha = ['!'..'~'] \\ wordAlpha

wordRange :: Parsec String () [Char]
wordRange = do
    lowerBound <- anyChar
    minusSingal
    upperBound <- anyChar
    return [lowerBound..upperBound]

singalWord :: Parsec String () [Char]
singalWord = (:[]) <$> noneOf "-[]|"

regRange :: Parsec String () [Char]
regRange = (nub . mconcat) <$> (brackets $ many (try wordRange <|> singalWord))

regRepeat :: Parsec String () Integer
regRepeat = braces integer' where
    integer' = read <$> (many1 $ oneOf ['0'..'9'])

escape :: Parsec String () (EscapeType Char)
escape = do
    char '\\'
    c <- anyChar
    return $ if 
        |c `elem` savedChars -> JustEscape c
        |c == 'w' -> WordAlpha
        |c == 'W' -> AntiWordAlpha
    
escapeToCharSet :: EscapeType Char -> [Char]
escapeToCharSet e = case e of
    JustEscape c -> [c]
    WordAlpha -> wordAlpha
    AntiWordAlpha -> antiWordAlpha

attachRepeat :: Parsec String () a -> Parsec String () [a]
attachRepeat p = do
    item <- p
    repeatTime <- many regRepeat
    let totalRepeatTime = product repeatTime
    return $ replicate (fromInteger totalRepeatTime) item
    
regRangeWithRepeat :: Parsec String () [[Char]]
regRangeWithRepeat = attachRepeat regRange

escapeWithRepeat :: Parsec String () [EscapeType Char]
escapeWithRepeat = attachRepeat escape

simpleParser :: Parsec String () [[[Char]]]
simpleParser = many $ simpleChars <|> (fmap escapeToCharSet) <$> escapeWithRepeat <|> regRangeWithRepeat where
    simpleChars = (:[]) <$> (:[]) <$> noneOf savedChars

