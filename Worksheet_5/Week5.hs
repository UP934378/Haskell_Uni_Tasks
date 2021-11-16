{- Week5.hs
 This file illustrates list patterns and recursion over lists.
-}

import Prelude hiding (fst, snd, head, tail, sum, concat, reverse, zip)

-- Definitions of the prelude functions fst and snd

fst (x,_)       = x
snd (_,y)       = y

-- Definitions of the prelude functions head and tail

head (x:_)      = x
tail (_:xs)     = xs

absFirst :: [Int] -> Int
absFirst []     = -1
absFirst (x:xs) = abs x

sum :: [Int] -> Int 
sum []     = 0
sum (x:xs) =   x + sum xs

doubleAll :: [Int] -> [Int]
doubleAll []      = []
doubleAll (x:xs)  = 2*x : doubleAll xs

concat :: [[a]] -> [a]
concat []         = []
concat (x:xs)     = x ++ concat xs

reverse :: [a] -> [a]
reverse []      = []
reverse (x:xs)  = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys)  = (x,y) : zip xs ys
zip _ _            = []

-- Question 1

headPlusOne :: [Int] -> Int
headPlusOne x 
    | null x == False = head x + 1
    | otherwise = 0

-- Question 2

duplicateHead :: [a] -> [a]
duplicateHead x
    | null x == False = head x : x 
    | otherwise = x

-- Question 3

rotate :: [a] -> [a]
rotate (a:b:rest) = b:a:rest
rotate a = a

-- Question 4

listLength :: [a] -> Int
listLength x = length x

-- Question 5

multAll :: [Int] -> Int
multAll x 
    | null x /= True = head x * multAll (tail x)
    | otherwise = 1


-- multAll [] = ????
-- multAll (x:xs) = ????

-- Question 6

andAll :: [Bool] -> Bool
andAll r
    | head r == False = False
    | head r == True && tail r /= [] = andAll (tail r)
    | otherwise = True

-- Question 7

countElems :: Int -> [Int] -> Int
countElems _ [] = 0
countElems x (l:xs)
    |x == l = 1 + countElems x xs
    | otherwise = countElems x xs

-- Question 8

removeAll :: Int -> [Int] -> [Int]
removeAll _ [] = []
removeAll x (l:xs) 
    | x == l = removeAll x xs
    | otherwise = l : removeAll x xs

-- Question 9
type StudentMark = (String, Int)

listMarks :: String -> [StudentMark] -> [Int]
listMarks _ [] = []
listMarks x ((a, b) : xs)
    | x == a = b : listMarks x xs
    | otherwise = listMarks x xs

-- Question 10

sorted :: [Int] -> Bool
sorted (x : y : list) = x <= y && sorted ( y : list)
sorted _ = True

-- Question 11

prefix :: [Int] -> [Int] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (a:ab) (b : bc) = a == b && prefix ab bc

-- Question 12
subSequence :: [Int] -> [Int] -> Bool
subSequence [] _ = True
subSequence _ [] = False
subSequence a (b:bs) = prefix a (b:bs) || subSequence a bs