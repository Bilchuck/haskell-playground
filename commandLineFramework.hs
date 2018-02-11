module CommandLineFramework where
import System.IO

readLineCycle :: (String -> IO String) -> (String -> Bool) -> IO String -> IO ()
readLineCycle fn exitFn rules = do
    rulesText <- rules
    _ <- putStrLn rulesText
    content <- getLine
    if  exitFn content
        then return ()
        else do
            result <- fn content
            readLineCycle fn exitFn rules
