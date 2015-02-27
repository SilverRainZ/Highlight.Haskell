--Lib Funciton 
--head
head' :: [a] -> a
head' [] = error "empty list."
head' (x:_) = x

--tail
tail' :: [a] -> [a]
tail' [] = error "empty list." 
tail' (_:xs) = xs

--last 
last' :: [a] -> a
last' [] = error "empty list." 
last' (x:[]) = x
last' (x:xs) = last' xs 

last'' :: [a] -> a
--last'' [] = error "empty list."
last'' = foldl1 (\_ x -> x)

--init 
init' :: [a] -> [a]
init' [] = error "empty list." 
init' (x:[]) = []
init' (x:xs) = x:init' xs

--sum
sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

sum'' :: (Num a) => [a] -> a
sum'' [] = 0
sum'' (x:xs) = x + sum'' xs

--length 
length' :: [a] -> Int
length' xs = sum[1 | _ <- xs]

length'' :: [a] -> Int
length'' = foldl (\x _ -> x + 1) 0 

--null
null' :: [a] -> Bool
null' [] = True
null' xs = False

--take
take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' a (x:xs) = x:take' (a - 1) xs

--drop
drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' a (x:xs) = drop' (a - 1) xs

--reverse
reverse' :: [a] -> [a]
--reverse' = foldl1 (\acc x -> x:acc)
reverse' = foldl (flip(:)) []

reverse'' :: [a] -> [a]
reverse'' [] = []
reverse'' (x:xs) = reverse'' xs ++ [x]

--maximum 
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list!"
maximum' (x:[]) = x
maximum' (x:xs) = max x $ maximum' xs

--minimum
minimum' :: (Ord a) => [a] -> a
minimum' [] = error "empty list!"
minimum' (x:[]) = x
minimum' (x:xs) = min x $ minimum' xs

--takeWhile
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' a (x:xs) = if a x then x:takeWhile' a xs else []

--flip why?
flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

--map do not need map' f xs = ... because of curring
map' :: (a -> b) -> [a] -> [b]
map' f = foldl (\acc x -> acc ++ [f x]) []

map'' :: (a -> b) -> [a] -> [b]
map'' _ [] = []
map'' f (x:xs) = f x : (map'' f xs)
--why can no f x : % map'' f x

--filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldl (\acc x -> if f x then acc ++ [x] else acc) []

