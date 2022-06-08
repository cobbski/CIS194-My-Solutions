-- CIS 194 - Homework 4

-- Exercise 1: Wholemeal Programming
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter (even)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter (even) . takeWhile (> 1) . iterate (\n -> if even n then n `div` 2 else 3*n+1)


-- Exercise 2: Folding with trees
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

insertTree :: a -> Tree a -> Tree a
insertTree it Leaf = Node 0 Leaf it Leaf
insertTree it (Node 0 l@Leaf v r) = Node 1 (insertTree it l) v r
insertTree it (Node 1 l v r@Leaf) = Node 1 l v (insertTree it r)
insertTree it (Node h t1@(Node h1 _ _ _) v t2@(Node h2 _ _ _))
    | h1 < h2  = Node h (insertTree it t1) v t2
    | h1 > h2  = Node h t1 v (insertTree it t2)
    | h1 == h2 = do
        let n@(Node nh _ _ _) = insertTree it t1
        if nh > h1
            then Node (h+1) n v t2
            else Node h n v t2 

foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf


-- Exercise 3: More folds!
{-
    1. Return True if and only if there are an odd number of True values in the input list.
    It does not matter how many False vaues the list contains.
-}
xorBinary :: Bool -> Bool -> Bool
xorBinary True True = False
xorBinary False False = False
xorBinary _ _ = True

xor :: [Bool] -> Bool
xor = foldr xorBinary False

-- |2. Implement map as a fold
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\it l -> f it : l) []


-- Exercise 4: Finding primes
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) $ filter (\n -> not (n `elem` (map (\(i, j) -> i + j + 2*i*j)
                  $ filter (\(i, j) -> i <= j && i + j + 2*i*j <= n) (cartProd [1..n] [1..n])))) [1..n]
