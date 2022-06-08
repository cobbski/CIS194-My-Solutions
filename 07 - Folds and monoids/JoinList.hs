-- CIS 194 - Homework 7
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module JoinList where
import Sized
import Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

-- Exercise 1
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jl1 jl2 = Append (mappend (tag jl1) (tag jl2)) jl1 jl2

-- Exercise 2
jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

count :: (Sized b, Monoid b) => JoinList b a -> Int
count = (getSize . size . tag)

indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ 0 (Single m a) = Just a
indexJ i (Append m jl1 jl2)
    | i < lcount             = indexJ i jl1
    | i < (getSize . size) m = indexJ (i - lcount) jl2
    where lcount = count jl1
indexJ _ _ = Nothing

dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
dropJ 0 jl = jl
dropJ n Empty = Empty
dropJ n (Single _ a) = Empty
dropJ n jl@(Append m jl1 jl2)
    | count jl <= n = Empty
    | lcount <= n   = dropJ (n-lcount) jl2
    | lcount > n    = (dropJ n jl1) +++ jl2
    where lcount = count jl1

takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
takeJ 0 jl = Empty
takeJ n Empty = Empty
takeJ n jl@(Single _ a) = jl
takeJ n jl@(Append m jl1 jl2)
    | count jl <= n = jl
    | lcount < n    = jl1 +++ takeJ (n - lcount) jl2
    | lcount == n   = jl1
    | lcount > n    = takeJ n jl1
    where lcount = count jl1

-- Exercise 3
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s
