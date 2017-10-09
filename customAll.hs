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