{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
module Main where
import StringParser
import System.Environment (getArgs)
import Text.Parsec (parse)
import Control.Monad.Random
import System.IO

charify :: MonadRandom m => Tok c -> m [c]
charify (Tok {..}) = concat <$> do
    n <- repeatTime getSuffix
    replicateM (fromInteger n) $ case getToken of
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

repeatTime :: MonadRandom m => Suffix -> m Integer
repeatTime s = case s of
    Nil -> return 1
    Repeat n -> return n
    RepeatRange (l, u) -> uniform [l..u]
    Some -> randomlyRise simpleRiseChance 1
    Many -> randomlyRise simpleRiseChance 0

randomlyRise :: MonadRandom m => Double -> Integer -> m Integer
randomlyRise chance n = do
    dice <- getRandom :: forall m2. MonadRandom m2 => m2 Double
    if dice < chance then
        randomlyRise chance (n+1)
    else return n

noInputMessage :: String
noInputMessage = "No args detached...\n" ++ 
                 "Entering interactive mode...\n"
    
mkRandomSyntax :: String -> IO String
mkRandomSyntax s = case parse mainParser "" s of
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
    hSetBuffering stdout NoBuffering
    case args of
        [] -> putStr noInputMessage >> forever mainInteract
        _ ->  forM args (\l -> mkRandomSyntax l >>= putStrLn) >> return ()

    
