-- CIS 194 - Homework 6
{-# LANGUAGE FlexibleInstances #-}
module Fibonacci where


-- Exercise 1
fib :: Integer -> Integer
fib n
    | (n == 0 || n == 1) = n
    | otherwise = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]


-- Exercise 2
fibs2Val :: Integer -> Integer -> [Integer]
fibs2Val x y = z : fibs2Val y z
         where z = x + y

fibs2 :: [Integer]
fibs2 = 0 : 1 : fibs2Val 0 1


-- Exercise 3
data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream s ss) = s : streamToList ss

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList


-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x xs) = Stream (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x (streamFromSeed f (f x))


-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x xs) (Stream y ys) = Stream x (Stream y (interleaveStreams xs ys))

rulerRepeat :: Integer -> Stream Integer
-- Unfortunately I couldn't come up with a way to get this to go to infinity without hanging
-- so I must cut it off here :(
rulerRepeat 100 = streamRepeat 0
rulerRepeat i = interleaveStreams (streamRepeat i) (rulerRepeat (i+1))

ruler :: Stream Integer
ruler = rulerRepeat 0

-- interleaveStreams (streamRepeat 0) (interleaveStreams (streamRepeat 1) (interleaveStreams (streamRepeat 2) (...)))


-- Exercise 6
x :: Stream Integer
x = Stream 0 (Stream 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger i = Stream i (streamRepeat 0)
    negate = streamMap negate
    (+) (Stream a as) (Stream b bs) = Stream (a + b) (as + bs)
    (*) a@(Stream a0 as) b@(Stream b0 bs) = Stream (a0*b0) (streamMap (* a0) bs + (as * b))

instance Fractional (Stream Integer) where
    (/) a@(Stream a0 as) b@(Stream b0 bs) = Stream (a0 `div` b0) (streamMap (* (1 `div` b0)) (as - (a/b)*bs))

fibs3 :: Stream Integer
fibs3 = Stream 0 (fromInteger 1) / Stream 1 (Stream (-1) (Stream (-1) (streamRepeat 0)))


-- Exercise 7
data Matrix = Matrix Integer Integer Integer Integer    -- represents a 2x2 matrix of integers
    deriving (Eq, Show)

instance Num Matrix where
    (*) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22) =
        Matrix (a11*b11+a12*b21) (a11*b12+a12*b22) (a21*b11+a22*b21) (a21*b12+a22*b22)

-- raise f_n to the n-th power to get the 2x2 matrix [F_n+1 F_n F_n F_n-1]
f_n :: Matrix
f_n = Matrix 1 1 1 0

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 i = a where (Matrix _ a _ _) = f_n ^ i
