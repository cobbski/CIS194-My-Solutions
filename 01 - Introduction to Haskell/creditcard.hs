-- CIS 194 - Homework 1

import Control.Monad
import System.Exit

-- sum all digits in array including double digit numbers
sumDigits :: [Int] -> Int
sumDigits []       = 0
sumDigits (x : xs) = x `div` 10 + x `mod` 10 + sumDigits xs

doubleEverySecond :: [Int] -> [Int]
doubleEverySecond []               = []
doubleEverySecond (x : [])         = [x]
doubleEverySecond (x : (x2 : x2s)) = x : 2*x2 : doubleEverySecond x2s

toDigits :: [Char] -> [Int]
-- toDigits s = map (read . (:"")) s :: [Int]
toDigits [] = []
toDigits (c : cs)
  | c `elem` "0123456789" = ((read . pure :: Char -> Int) c) : toDigits cs
  | otherwise             = []

validate :: String -> IO ()
validate cardNum = do
  let digits = toDigits cardNum
  when ((length digits) /= 16) $
    do putStrLn "Invalid Credit Card (must be 16 digits)"
       exitWith (ExitFailure 1)

  let doubledFromRight = reverse (doubleEverySecond (reverse digits))
  let checkSum = (sumDigits doubledFromRight) `mod` 10
  if checkSum == 0
    then putStrLn "Validation Successful!"
    else do putStrLn ("Invalid Credit Card (checksum failed with " ++ show checkSum ++ ")")
            exitWith (ExitFailure 2)

main :: IO ()
main = do
  putStrLn "Please enter credit card number for validation:"
  cardNum <- getLine
  validate cardNum

  exitWith (ExitSuccess)
