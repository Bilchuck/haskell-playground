-- binary tree

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Ord, Eq)

treeElem :: (Eq a, Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem val (Node nodeVal left right) = case val `compare` nodeVal of
    EQ -> True
    LT -> treeElem val left 
    GT -> treeElem val right

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

-- treeElem 12 tree'

singelton :: a -> Tree a
singelton x = Node x EmptyTree EmptyTree

tree'' = Node 5 
    (Node 2 (singelton 1) (singelton 3))
    (Node 10 (singelton 8) (singelton 12))


treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert el EmptyTree = singelton el
treeInsert el tree@(Node x left right) = case el `compare` x of 
    EQ -> tree
    LT -> Node x (treeInsert el left) right
    GT -> Node x left (treeInsert el right)

-- treeInsert 6 tree''