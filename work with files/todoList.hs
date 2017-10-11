import System.IO
import System.IO.Error
import Data.List
import Data.Char

main = do 
    rules <- showRule
    _ <- putStrLn rules
    interactIo proccessInput

interactIo :: (String -> IO String) -> IO ()
interactIo f = do
    s <- getLine
    if s == "quit" 
        then return () 
        else do
            r <- f s
            _ <- putStrLn r
            interactIo f

proccessInput :: String -> IO String
proccessInput ('A':'d':'d':xs) = do
    _ <- appendFile "todos.txt" $ "\n" ++ xs
    _ <- putStrLn $ "appendFile todos.txt $ /n " ++ xs
    showRule
proccessInput _ = return ""

showRule = do
    contents <- readFile "todos.txt"
    return $ displayTodos contents ++ "\n" ++ "Add:{string} to add todo"

displayTodos :: String -> String
displayTodos = unlines . displayTodos' 1 . lines

displayTodos' :: Int -> [String] -> [String]
displayTodos' index (x: xs) = (show index ++ ". " ++ x) : (displayTodos' (index + 1) xs)
displayTodos' _ [] = []