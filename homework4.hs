module WholeMeal where


import Debug.Trace

-- Exercise 1
fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter (even)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n    = trace (show n) $ n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter (even) . takeWhile (/=1) . iterate (\x -> if even x then (x `div` 2) else ((3 * x) + 1))

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\acc x -> f x acc) base xs

-- Exercise 2

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
    where
        insert :: a -> Tree a -> Tree a
        insert t Leaf = Node 0 Leaf t Leaf
        insert t (Node _ left value right)
            | nodeHeight left <= nodeHeight right =
                let lleft = insert t left
                in Node (nodeHeight lleft + 1) (lleft) value right
            | otherwise =
                let rright = insert t right
                in Node (nodeHeight rright + 1) left value rright

nodeHeight :: Tree a -> Integer
nodeHeight Leaf = -1
nodeHeight (Node nHeight _ _ _) = nHeight

-- Exercise 3

xor :: [Bool] -> Bool
xor = odd . foldl (\acc x -> if x == True then (acc + 1) else acc) 0

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\acc x -> (f acc):x) []

sieveSundaram :: Integer -> [Integer]
sieveSundaram = primes

primes :: Integer -> [Integer]
primes n = take (fromIntegral(2 * n + 1)) $ filter (odd)  $ sieve [3..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]