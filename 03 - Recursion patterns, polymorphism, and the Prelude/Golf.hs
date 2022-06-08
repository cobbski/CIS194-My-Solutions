-- CIS 194 - Homework 3
module Golf where


-- Exercise 1: Hopscotch
-- gets every t-th element of arr
every :: Int -> Int -> [a] -> [a]
every _ _ [] = []
every t 0 (x : xs) = x : every t t xs
every t cnt (_ : xs) = every t (cnt-1) xs

everyT :: Int -> [a] -> [[a]]
everyT 1 l = [l]
everyT t l = every (t-1) (t-1) l : everyT (t-1) l

skips :: [a] -> [[a]]
skips l = everyT (length l) l


-- Exercise 2: Local maxima
localMaxima :: [Int] -> [Int]
localMaxima l | length l <= 2 = []
localMaxima (a:l@(b:c:_))
    | b > a && b > c = b : localMaxima l
    | otherwise      = localMaxima l


-- Exercise 3: Histogram
-- test expr: (putStr . histogram) (take 25 $ cycle (filter (odd) [0..9]))
count :: [Int] -> Int -> Int
count l n = (length . filter (==n)) l

getStar :: Int -> Char
getStar n
    | n > 0     = '*'
    | otherwise = ' '

getCountStr :: [Int] -> String
getCountStr l
    | any (> 0) l = getCountStr (map (subtract 1) l) ++ map getStar l ++ "\n"
    | otherwise  = ""

histogram :: [Int] -> String
histogram l = getCountStr (map (count l) [0..9]) ++ "0123456789\n"
