module TreeProgam where
    
import System.Environment
import System.Directory
import System.IO
import Data.List
import Tree
    
main = do
    putStrl "Write tree size"
    number <- getLine
    file <- openFile "tree.txt" WriteMode
    hPutStr file $ treeStr number

treeStr :: String -> String
treeStr numberStr = treeStr' tree
    where number = read numberStr
          tree = foldr treeInsert EmptyTree [1..number]

treeStr' :: Tree Int -> String
treeStr' (Node x left right) = 
    spaces x ++ 
    (show x) ++ 
    "\n" ++ 
    treeStr' left ++ 
    treeStr' right

spaces :: Int -> String
spaces n = foldr (++ " ") "" [1..n]