import Data.List

data SqValue = X | O | SqSpace deriving (Eq, Show)

type Field = [[SqValue]] 

-- display 
display :: Field -> String
display = intercalate "\n" . map displayRow

displayRow :: [SqValue] -> String
displayRow = intercalate " | " . map displaySq

displaySq :: SqValue -> String
displaySq X     = "X"
displaySq O     = "O"
displaySq SqSpace = " "

-- generator
generateField :: Int -> Field
generateField n = map (\_-> map (\_ -> SqSpace) [1..n]) [1..n]

-- main = do
--     putStr $ displaySq $ generateField 2