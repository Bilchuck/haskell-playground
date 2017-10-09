data Vector a = Vector a a deriving (Show)

addV :: (Num t) => Vector t -> Vector t -> Vector t
Vector x1 y1 `addV` Vector x2 y2 = Vector (x1 + x2) (y1 + y2)
