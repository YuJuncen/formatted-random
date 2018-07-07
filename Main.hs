module Main where
import StringParser
import System.Environment (getArgs)
import Text.Parsec (parse)
import Control.Monad.Random

noInputMessage :: String
noInputMessage = "No args detached...\n" ++ 
                 "Entering interactive mode...\n"
    

mkRandomSyntax :: String -> IO String
mkRandomSyntax s = case parse simpleParser  "" s of
    Left err -> return $ show err
    Right res -> (fmap concat. sequence . fmap charify) res 

mainInteract :: IO ()
mainInteract = do
    putStr "> "
    line <- getLine
    cho <- mkRandomSyntax line
    putStrLn cho

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStr noInputMessage >> forever mainInteract
        _ ->  forM args (\l -> mkRandomSyntax l >>= putStrLn) >> return ()

    
