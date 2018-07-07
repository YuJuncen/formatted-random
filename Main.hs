module Main where
import StringParser
import Text.Parsec (parse)
import Control.Monad.Random

mkRandomString :: String -> IO [String]
mkRandomString s = case parse simpleParser' "" s of
    Left err -> return [show err]
    Right s -> return s
    where
    simpleParser' = do
        res <- simpleParser
        let res' = concat res
        return res'

main :: IO ()
main = do
    line <- getLine
    dealed <- mkRandomString line
    cho <- sequence $ uniform <$> dealed
    putStrLn cho
