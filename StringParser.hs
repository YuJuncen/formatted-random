{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module StringParser where

import Text.Parsec
import Control.Monad.Random
import Text.Parsec.Char
import Data.List(nub, (\\))
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

data EscapeType c = Printable | AntiAlpha | Aplha | AntiNum | Num | NumAlpha | AntiNumAlpha | JustEscape c
    deriving (Show, Eq)
data RegToken c = CharRange [c] | SingalChar c | SubExpr [Tok c] | MultiWaySubExpr [[Tok c]]
    deriving (Show, Eq)
data Suffix = Repeat Integer | RepeatRange (Integer, Integer) | Some | Many | Nil 
    deriving(Show, Eq)
data Tok c = Tok {
    getToken :: RegToken c,
    getSuffix :: Suffix
} deriving (Eq)

instance Show c => Show(Tok c) where
    show (Tok {..}) = show getToken ++ " {" ++ show getSuffix ++ "}\n"

-- the build-in parsers.
lexer = P.makeTokenParser haskellDef
integer = P.integer lexer
braces = P.braces lexer
parens = P.parens lexer
brackets = P.brackets lexer
commaSep1 = P.commaSep1 lexer
minusSingal = char '-'

-- some configures.
simpleRiseChance = 0.7
savedChars = "\\[]~{}()|?+"

-- Escaped char range.
-- use the printable as global char set.
printable = [' '..'~']
numAlpha = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
antiNumAlpha = printable \\ numAlpha
alpha = ['a'..'z'] ++ ['A'..'Z']
antiAlpha = printable \\ alpha
num = ['0'..'9']
antiNum = printable \\ num

-- parse about [*pattern*] syntax
_tokenMerge :: [RegToken c] -> [c]
_tokenMerge [] = []
_tokenMerge (SingalChar c: ts) = c  :  _tokenMerge ts
_tokenMerge (CharRange cs: ts) = cs ++ _tokenMerge ts
_tokenMerge (_: ts)            = _tokenMerge ts

wordRange :: Parsec String () (RegToken Char)
wordRange = do
    lowerBound <- anyChar
    minusSingal
    upperBound <- anyChar
    return $ CharRange [lowerBound..upperBound]

singalWord :: Parsec String () (RegToken Char)
singalWord = SingalChar<$> noneOf "-[]"

regRange :: Parsec String () (RegToken Char)
regRange = (CharRange . nub . _tokenMerge ) <$> (brackets $ many (try wordRange <|> singalWord))

regRepeat :: Parsec String () Integer
regRepeat = braces integer' where
    integer' = read <$> (many1 $ oneOf ['0'..'9'])

-- parse \*escape_char* syntax
escape :: Parsec String () (EscapeType Char)
escape = do
    char '\\'
    c <- anyChar
    return $ if 
        |c `elem` savedChars -> JustEscape c
        |c == 'w' -> NumAlpha
        |c == 'W' -> AntiNumAlpha
        |c == 'a' -> Aplha
        |c == 'A' -> AntiAlpha
        |c == 'd' -> Num
        |c == 'D' -> AntiNum
        |c == 'p' -> Printable
    
escapeToCharSet :: EscapeType Char -> RegToken Char
escapeToCharSet e = case e of
    JustEscape c -> SingalChar c
    NumAlpha -> CharRange numAlpha
    AntiNumAlpha -> CharRange antiNumAlpha
    Aplha -> CharRange alpha
    Num -> CharRange num
    AntiAlpha -> CharRange antiAlpha
    AntiNum -> CharRange antiNum
    Printable -> CharRange printable

-- parse (*substr*|*substr*|...) syntax
subExpr :: Parsec String () (RegToken Char)
subExpr =  SubExpr <$> parens mainParser

multiWaySubexpr :: Parsec String () (RegToken Char)
multiWaySubexpr = MultiWaySubExpr <$> (parens $ sepBy mainParser (char '|'))

suffixSome :: Parsec String () Suffix
suffixSome = char '+' >> return Some

suffixMany :: Parsec String () Suffix
suffixMany = char '*' >> return Many

suffixRepeat :: Parsec String () Suffix
suffixRepeat = braces innerParser where 
    innerParser = do
        ns <- commaSep1 integer
        if length ns > 1 then do
            let (lower: upper: _) = ns
            return $ RepeatRange (lower, upper)
        else do
            let (n: _) = ns
            return $ Repeat n

-- main parsers
tokenParser :: Parsec String () (Tok Char)
tokenParser = do
    tok <- simpleChar <|> regRange <|> (escapeToCharSet <$> escape) <|> try subExpr <|> multiWaySubexpr
    suf <- suffixMany <|> suffixSome <|> try suffixRepeat <|> pure Nil
    return $ Tok tok suf where
        simpleChar = SingalChar <$> noneOf savedChars

mainParser :: Parsec String () [Tok Char]
mainParser  = many tokenParser