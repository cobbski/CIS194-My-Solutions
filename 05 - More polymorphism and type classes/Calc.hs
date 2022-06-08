-- CIS 194 - Homework 5

{-# LANGUAGE FlexibleInstances #-}
module Calc where
import Parser
import StackVM


-- Exercise 1
data ExprT = Lit Integer
           | AddT ExprT ExprT
           | MulT ExprT ExprT
    deriving (Show, Eq)

eval :: ExprT -> Integer
eval (Lit i)   = i
eval (AddT x y) = eval x + eval y
eval (MulT x y) = eval x * eval y


-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr s = maybe Nothing (Just . eval) (parseExp Lit AddT MulT s)


-- Exercise 3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = AddT
    mul = MulT


-- Exercise 4
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit i   = i > 0
    add x y = x || y
    mul x y = x && y

newtype MinMax = MinMax Integer deriving (Show, Eq)

instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y) = MinMax $ max x y
    mul (MinMax x) (MinMax y) = MinMax $ min x y

newtype Mod7 = Mod7 Integer deriving (Show, Eq)

instance Expr Mod7 where
    lit = Mod7
    add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
    mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7


-- Exercise 5
instance Expr Program where
    lit i = [PushI i]
    add x y = x ++ y ++ [Add]
    mul x y = x ++ y ++ [Mul]

compile :: String -> Maybe Program
compile exp = parseExp lit add mul exp :: Maybe Program