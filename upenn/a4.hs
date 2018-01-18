import Data.List

-- fun1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

f1 :: [Integer] -> Integer
f1 = foldr (*) 1 . map (\x -> x-2) . filter (even)


-- fun2

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

f2 = sum . filter even . takeWhile (>=2) . (iterate (\x -> if even x then (x `div` 2) else (3*x+1)))


-- foldTree

data Tree a = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr makeTree Leaf

makeTree :: a -> Tree a -> Tree a
makeTree a Leaf = Node 0 Leaf a Leaf
makeTree a (Node h l m r)
    | height l <= height r = 
        let left = makeTree a l
        in Node (height left + 1) left m r
    | otherwise =
        let right = makeTree a r
        in Node (height right + 1) l m right
    where
        height Leaf = -1
        height (Node h _ _ _) = h


-- xor

xor :: [Bool] -> Bool
xor = not . even . length . filter (\x -> x) 


-- map'

map' :: (a->b) -> [a] -> [b]
map' f = foldr (\x y -> (f x) : y) []


-- myFoldl

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x s -> f s x) base (reverse xs)


-- sieveSundaram

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

genCross :: Integer -> [Integer]
genCross n = 
    let half = (n + 1) `div` 2
    in map (\(i,j) -> i + j + 2 * i * j) (filter valid (cartProd [1..half] [1..half]))
    where valid (i, j) = (i + j + 2 * i * j) <= n && i <= j

genSieve :: Integer -> [Integer]
genSieve n = (\\) [1..n] (genCross n)

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2 * x + 1) (genSieve n)
