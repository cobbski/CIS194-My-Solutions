-- CIS 194 - Homework 8
import Data.Tree
import Data.List
import Employee

-- Exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (f + empFun e)

instance Semigroup GuestList where
    (<>) (GL e1 f1) (GL e2 f2) = GL (e1 ++ e2) (f1 + f2)

instance Monoid GuestList where
    mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2 = if gl1 > gl2 then gl1 else gl2


-- Exercise 2
treeFold :: b -> ([b] -> a -> b) -> Tree a -> b
treeFold e f (Node rootLabel subForest) = f (map (treeFold e f) subForest) rootLabel


-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)]
             -> (GuestList, GuestList)
nextLevel emp [] = (GL [emp] (empFun emp), GL [] 0)
nextLevel boss bestLists = (
    glCons boss $ foldl (\gl n -> gl <> snd n) mempty bestLists,
    foldl (\gl n -> gl <> uncurry moreFun n) mempty bestLists
    )


-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun t = uncurry moreFun $ treeFold mempty (\gls boss -> nextLevel boss gls) t


-- Exercise 5
namesList :: GuestList -> [String]
namesList = map empName . empList

main :: IO ()
main = do
    fstr <- readFile "company.txt"
    let company = read fstr :: Tree Employee
    let gl = maxFun company
    putStrLn ("Total fun: " ++ show (totalFun gl))
    putStrLn $ (intercalate "\n" . sort) (namesList gl)
