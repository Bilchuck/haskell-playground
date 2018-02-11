module CustomArrayFunc where

foldl' :: (c -> a -> c) -> c -> [a] -> c
foldl' _ start [] = start
foldl' fn value (x:xs) = foldl' fn (fn value x) xs

foldr' :: (a -> c -> c) -> c -> [a] -> c
foldr' _ start [] = start
foldr' fn value (x:xs) = fn x (foldr' fn value xs)

foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' fn (x:xs) = foldr' fn x xs

map' :: (a -> b) -> [a] -> [b]
map' fn = foldr' (\x xs -> fn x : xs) []

sum' :: (Num a) => [a] -> a
sum' = foldr' (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl' (\acc x -> if x == y then True else acc) False ys

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1' (\x acc -> if x > acc then x else acc)

minimum':: (Ord a) => [a] -> a
minimum' = foldr1' (\x acc -> if x < acc then x else acc)

-- QUICK SORT

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted
  

reverse' :: [a] -> [a]
reverse' = foldr(\x acc -> acc ++ [x]) []

-- O(n)
haveSumSortedList :: Int -> [Int] -> Bool
haveSumSortedList _ [] = False
haveSumSortedList _ [x] = False
haveSumSortedList sum list@(x: xs) = case lastPlusFirst `compare` sum of 
    EQ -> True
    LT -> haveSumSortedList sum xs
    GT -> haveSumSortedList sum (init list)
    where lastPlusFirst = x + last (xs)
-- haveSumSortedList 10 [1,2,3,4,6]

-- O(n) but with cache
haveSumList :: Int -> [Int] -> Bool
haveSumList sum xs = haveSumList' sum xs []

haveSumList' :: Int -> [Int] -> [Int] -> Bool
haveSumList' _ [] _     = False
haveSumList' _ [x] _    = False
haveSumList' sum (x:xs) cache = if x `elem` cache 
    then True 
    else haveSumList' sum xs (extractSum x : cache)
    where extractSum = ((-) sum) 
-- haveSumList 10 [100, 2, 1, 1, 4, 3, 6, 20]

delIndex' :: Int -> [a] -> [a]
delIndex' n xs 
    | n < 0 = xs
    | otherwise = (take' n xs) ++ (drop' (n + 1) xs)

take' :: Int -> [a] -> [a]
take' n (x:xs)
    | n == 0 = []
    | n < 0 = []
    | n > 0 = x:(take' (n - 1) xs)

drop' :: Int -> [a] -> [a]
drop' n all@(_:xs)
    | n == 0 = all
    | n < 0 = []
    | n > 0 = drop' (n - 1) xs