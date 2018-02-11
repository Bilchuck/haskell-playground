-- data List a = Empty | Cons a (List a) deriving (Show, Eq, Ord, Read)

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Eq, Ord, Read)

head' :: List a -> a
head' (x :-: xs) = x

last' :: List a -> a
last' (x :-: Empty) = x
last' (x :-: xs) = last' xs

foldr' :: (a -> b -> b) -> b -> List a -> b
foldr' f value (x :-: xs) = foldr' f (f x value) xs
foldr' _ start Empty = start

foldl' :: (b -> a -> b) -> b -> List a -> b
foldl' f value (x :-: xs) = (f (foldl' f value xs) x)
foldl' _ start Empty = start

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ list = list
(x :-: xs) .++ list = x :-: (xs .++ list)

map' :: (a -> b) -> List a -> List b
map' f = foldl' (\v x -> f x :-: v) Empty
