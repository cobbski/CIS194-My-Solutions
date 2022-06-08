-- CIS 194 - Homework 1

import Text.Printf

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi discs peg1 peg2 peg3 
    | discs <= 0 = []
    | otherwise  = (hanoi (discs-1) peg1 peg3 peg2) ++ [(peg1, peg3)] ++ (hanoi (discs-1) peg2 peg1 peg3)

-- prints a list of moves (start, end) provided with their step number (n)
printMoves :: [(Int, Move)] -> IO ()
printMoves [] = return ()
printMoves ((n, (start, end)) : moves) = do
    printf "%d. %s -> %s\n" n start end
    printMoves moves

main :: IO ()
main = do
    putStrLn "Please enter the number of discs:"
    in1 <- getLine
    let discs = read in1 :: Integer
    let moves = hanoi discs "a" "b" "c"
    printf "Solution for %d discs:\n" discs
    printMoves (zip [1..] moves)
