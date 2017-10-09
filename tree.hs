data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Ord, Eq)

search :: (Eq a, Ord a) => a -> Tree a -> Bool
search _ EmptyTree = False
search val (Node nodeVal left right) = case val `compare` nodeVal of
    EQ -> True
    LT -> search val left 
    GT -> search val right

tree' = (Node 5 
            (Node 2 
                (Node 1 EmptyTree EmptyTree)
                (Node 3 EmptyTree EmptyTree)
            )
            
            (Node 10 
                (Node 8 EmptyTree EmptyTree)
                (Node 12 EmptyTree EmptyTree)
            )
        )