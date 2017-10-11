module HeathrowToLondon
( Section(..)
, Label(..)
, optimalPath
) where

data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Eq, Show)
type RoadSystem = [Section]
data Shortest = Shortest Path Path deriving (Eq, Show)

data Label = A | B | C deriving (Eq, Show)  
type Path = [(Label, Int)]

optimalPath' :: RoadSystem -> [Shortest]
optimalPath' = scanr pickPath (Shortest [] [])
  where pickFinal (Shortest pa pb) = if pathLength pa <= pathLength pb then pa else pb

optimalPath :: RoadSystem -> Path
optimalPath = pickFinal . foldr pickPath (Shortest [] [])
  where pickFinal (Shortest pa pb) = if pathLength pa <= pathLength pb then pa else pb

pickPath :: Section -> Shortest -> Shortest
pickPath (Section a b c) (Shortest pa pb) = Shortest a' b'
  where la = pathLength pa
        lb = pathLength pb
        a' = (A, a) : if la <= c + lb then pa else (C,c):pb
        b' = (B, b) : if lb <= c + la then pb else (C,c):pa

pathLength :: Path -> Int
pathLength = sum . map snd

-- optimalPath [Section 50 49 30, Section 10 100 20]