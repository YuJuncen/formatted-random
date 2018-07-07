module Main where
import StringParser
import System.Environment (getArgs)
import Text.Parsec (parse)
import Control.Monad.Random

noInputMessage :: String
noInputMessage = "No args detached...\n" ++ 
                 "Entering interactive mode...\n"

mkRandomString :: String -> IO [String]
mkRandomString s = case parse simpleParser' "" s of
    Left err -> return [show err]
    Right s -> return s
    where
    simpleParser' = do
        res <- simpleParser
        let res' = concat res
        return res'

mainInteract :: IO ()
mainInteract = do
    putStr "> "
    line <- getLine
    cho <- mkRandomStringFrom line
    putStrLn cho

mkRandomStringFrom :: String -> IO String
mkRandomStringFrom s = do
    dealed <- mkRandomString s
    cho <- sequence $ uniform <$> dealed
    return cho

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStr noInputMessage >> forever mainInteract
        _ ->  forM args (\l -> mkRandomStringFrom l >>= putStrLn) >> return ()

    
