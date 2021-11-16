import Data.Char

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1,m1) (s2,m2) 
    | m1 >= m2          = s1
    | otherwise         = s2

marks:: [StudentMark] -> [Int]
marks stmks = [ mk | (st,mk) <- stmks ]

pass :: [StudentMark] -> [String]
pass stmks = [ st | (st,mk) <- stmks, mk >= 40 ]

-- An example list of student marks
testData :: [StudentMark]
testData = [("John", 53), ("Sam", 16), ("Kate", 85), ("Jill", 65),
            ("Bill", 37), ("Amy", 22), ("Jack", 41), ("Sue", 71)]

addPairs :: [(Int,Int)] -> [Int]
addPairs pairList = [ i+j | (i,j) <- pairList ]

minAndMax :: Int -> Int -> (Int,Int)
minAndMax x y 
    | x <= y            = (x,y)
    | otherwise         = (y,x)

-- Tuples
-- Question 1

sumDifference :: Int -> Int -> (Int,Int)
sumDifference x y = ( x + y, x -y )


-- Question 2

grade :: StudentMark -> Char
grade (s1,m1)
    | m1 >= 70 = 'A'
    | m1<= 69  = 'B'
    | m1<= 59  = 'C'
    | m1<= 49  = 'D'
    | otherwise = 'F'

-- Question 3

capMark :: StudentMark -> StudentMark
capMark (s1,m1)
    | m1 < 40 = (s1,m1)
    | otherwise = (s1, 40)

-- Lists and Strings
-- Question 4

firstNumbers :: Int -> [Int]
firstNumbers x = [1 .. x]

-- Question 5

firstSquares :: Int -> [Int]
firstSquares x = [x ^ 2 | x <- [1 .. x]]

-- Question 6

capitalise :: String -> String
capitalise x = [toUpper i | i <- x]

-- Question 7

onlyDigits :: String -> String
onlyDigits x = [ i | i <- x, isDigit i]

-- Question 8

capMarks :: [StudentMark] -> [StudentMark]
capMarks x = [capMark (s1, m1) | (s1,m1) <- x]

-- Question 9 

gradeStudents :: [StudentMark] -> [(String,Char)]
gradeStudents x = [(s1, grade (s1, m1)) | (s1,m1) <- x]

-- Question 10

duplicate:: String -> Int -> String
duplicate str x 
    | x <= 0    = []  
    | otherwise = str ++ duplicate str (x - 1)

-- Question 11

divisors :: Int -> [Int]
divisors x = [i | i <- [1..x], x `mod` i == 0]

-- Question 12

isPrime :: Int -> Bool
isPrime x = length (divisors x) == 2

-- Question 13

split:: [(a,b)] -> ([a],[b])
split x = ([ a|(a,_) <- x], [b | (_,b) <- x])
