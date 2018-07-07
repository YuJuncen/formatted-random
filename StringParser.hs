{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
module StringParser where

import Text.Parsec
import Control.Monad.Random
import Text.Parsec.Char
import Data.List(nub, (\\))
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

data EscapeType c = NumAlpha | AntiNumAlpha | JustEscape c
    deriving (Show, Eq)
data RegToken c = CharRange [c] | SingalChar c | SubExpr [RegToken c] | MultiWaySubExpr [[RegToken c]]
    deriving (Show, Eq)
data Suffix = Repeat Integer | RepeatRange (Integer, Integer) | Some | Many | Nil

suffixparser :: Parsec String () Suffix
suffixparser = undefined

charify :: RegToken c -> IO [c]
charify t = case t of
    SingalChar      c -> 
        return [c]

    CharRange       cs -> do
        c <- uniform cs
        return [c]

    SubExpr         ts -> 
        concat <$> (sequence $ charify <$> ts)

    MultiWaySubExpr tss -> do 
        ts <- uniform tss
        concat <$> (sequence $ charify <$> ts)

instance Functor(RegToken) where
    -- fmap :: (a -> b) -> RegToken a -> RegToken b
    fmap f r = case r of
        CharRange cs -> CharRange $ fmap f cs
        SingalChar c -> SingalChar $ f c
        SubExpr   es -> SubExpr $ fmap f <$> es

lexer = P.makeTokenParser haskellDef
integer = P.integer lexer
braces = P.braces lexer
parens = P.parens lexer
brackets = P.brackets lexer
minusSingal = char '-'
savedChars = "\\[]~{}()|"
intToCh :: Int -> Char
intToCh = toEnum
numAlpha = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
antiNumAlpha = ['!'..'~'] \\ numAlpha

_tokenMerge :: [RegToken c] -> [c]
_tokenMerge [] = []
_tokenMerge (SingalChar c: ts) = c  :  _tokenMerge ts
_tokenMerge (CharRange cs: ts) = cs ++ _tokenMerge ts
_tokenMerge (_: ts)            = _tokenMerge ts

wordRange :: Parsec String () [RegToken Char]
wordRange = do
    lowerBound <- anyChar
    minusSingal
    upperBound <- anyChar
    return $ [CharRange [lowerBound..upperBound]]

singalWord :: Parsec String () [RegToken Char]
singalWord = ((:[]) . SingalChar) <$> noneOf "-[]|"

regRange :: Parsec String () [RegToken Char]
regRange = ((:[]) . CharRange . nub . _tokenMerge . concat) <$> (brackets $ many (try wordRange <|> singalWord))

regRepeat :: Parsec String () Integer
regRepeat = braces integer' where
    integer' = read <$> (many1 $ oneOf ['0'..'9'])

escape :: Parsec String () [EscapeType Char]
escape = (:[]) <$> do
    char '\\'
    c <- anyChar
    return $ if 
        |c `elem` savedChars -> JustEscape c
        |c == 'w' -> NumAlpha
        |c == 'W' -> AntiNumAlpha
    
escapeToCharSet :: EscapeType Char -> [RegToken Char]
escapeToCharSet e = (:[]) $ case e of
    JustEscape c -> SingalChar c
    NumAlpha -> CharRange numAlpha
    AntiNumAlpha -> CharRange antiNumAlpha

attachRepeat :: Parsec String () [a] -> Parsec String () [a]
attachRepeat p = do
    item <- p
    repeatTime <- many regRepeat
    let totalRepeatTime = product repeatTime
    return $ concat (replicate (fromInteger totalRepeatTime) item)
    
regRangeWithRepeat :: Parsec String () [RegToken Char]
regRangeWithRepeat = attachRepeat regRange

escapeWithRepeat :: Parsec String () [EscapeType Char]
escapeWithRepeat = attachRepeat escape

subExpr :: Parsec String () [RegToken Char]
subExpr =  attachRepeat $ ((:[]) . SubExpr) <$> parens simpleParser

multiWaySubexpr :: Parsec String () [RegToken Char]
multiWaySubexpr = attachRepeat $ (:[]) . MultiWaySubExpr <$> (parens $ sepBy simpleParser (char '|'))

simpleParser :: Parsec String () [RegToken Char]
simpleParser = concat <$> (many $ simpleChars <|> (concat . fmap escapeToCharSet) <$> escapeWithRepeat <|> regRangeWithRepeat <|> try subExpr <|>multiWaySubexpr) where
    simpleChars = ((:[]) . SingalChar) <$> noneOf savedChars
