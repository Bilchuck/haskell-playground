import System.IO
import System.IO.Error
import Data.List
import Data.Char
import CommandLineFramework
import CustomArrayFunc

filePath = "txtFiles/todos.txt"

main = readLineCycle proccessInput onQuitText showRule

data Command = Add String | Delete Int | Update Int String | Invalid

onQuitText :: String -> Bool
onQuitText = (==) ":q"

proccessInput :: String -> IO String
proccessInput xs = case (processCommand xs) of
    Add str -> do
        _ <- appendFile filePath $ "\n" ++ (trim str)
        _ <- putStrLn $ "appendFile todos.txt $ /n " ++ str
        return $ str ++ " added"
    Delete id -> do
        fileContent <- readFile filePath
        result <- return $ unlines . delIndex' (id - 1) . lines $ fileContent
        _ <- length result `seq` (writeFile filePath $ result)
        return $ (show id) ++ " element deleted"
    otherwise -> return ""

processCommand :: String -> Command
processCommand xs 
    | strBeginWith "Add:" xs = Add (sliceFirst "Add:" xs) 
    | strBeginWith "Del:" xs = Delete (read $ sliceFirst "Del:" $ xs) 
    | otherwise = Invalid

strBeginWith :: String -> String -> Bool
strBeginWith begin str = begin == (take (length begin) str)

sliceFirst :: String -> String -> String
sliceFirst begin str = drop (length begin) str

showRule = do
    contents <- readFile filePath
    return $ 
        displayTodos contents ++ 
        "\n" ++ 
        "Commands:" ++ 
        "\n" ++ 
        "| :q - to quit " ++
        "| Add:{string} - to add todo" ++
        "| Del:{index} - to remove todo"

displayTodos :: String -> String
displayTodos = unlines . displayTodos' 1 . filter ((> 0) . length) . lines

displayTodos' :: Int -> [String] -> [String]
displayTodos' index (x: xs) = (show index ++ ". " ++ x) : (displayTodos' (index + 1) xs)
displayTodos' _ [] = []

trim xs = dropSpaceTail "" $ dropWhile isSpace xs

dropSpaceTail maybeStuff "" = ""
dropSpaceTail maybeStuff (x:xs)
        | isSpace x = dropSpaceTail (x:maybeStuff) xs
        | null maybeStuff = x : dropSpaceTail "" xs
        | otherwise       = reverse maybeStuff ++ x : dropSpaceTail "" xs
