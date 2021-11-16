-- We don't import '||' from the prelude, so that we can 
-- define our own version

import Prelude hiding ((||), (&&), gcd) 
-- The following line declares the || operator (which we are about to
-- re-define) to be right associative and to have precedence 2. This
-- is necessary in order for expressions such as False || x > 2 to be
-- valid (e.g. it sets the precedence of || to be lower than >). 

--infixr 2  ||

-- A naive re-implementation of the Prelude operator ||
-- (||) :: Bool -> Bool -> Bool
-- True || True    = True
-- False || True   = True
-- True || False   = True
-- False || False  = False

-- An alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--False || False   = False
--_ || _           = True

-- Another alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--True || _     =  True
--False || a    = a


fact :: Int -> Int 
fact n 
    | n == 0    = 1
    | n > 0     = n * fact (n - 1)
    | otherwise = error "factorials not defined for negative ints"

mult :: Int -> Int -> Int
mult n m 
    | n == 0        = 0
    | n > 0         = m + mult (n - 1) m 

divide :: Int -> Int -> Int
divide n m
    | n < m         = 0
    | otherwise     = 1 + divide (n - m) m

-- QUESTIONS

-- QUESTION 1

infixr 3 &&

-- A naive re-implementation of the Prelude operator &&
(&&) :: Bool -> Bool -> Bool
True && True    = True
False && True   = False
True && False   = False
False && False  = False

-- -- An alternative re-implementation
-- (&&) :: Bool -> Bool -> Bool
-- True && True  = True
-- _ && _     = False

-- -- Another alternative re-implementation
-- (&&) :: Bool -> Bool -> Bool
-- True && a     =  a
-- False && _    = False

-- QUESTION 2

exOr :: Bool -> Bool -> Bool
exOr True a = not a
exOr False a = a

-- Question 3

ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True  a _ = a
ifThenElse False _ b = b

-- Question 4

daysInMonth :: Int -> Int
daysInMonth 2 = 28
daysInMonth 4 = 30
daysInMonth 6 = 30 
daysInMonth 9 = 30
daysInMonth 11 = 31
daysInMonth _ = 31

-- Question 5

sumNumbers :: Int -> Int
-- sumNumbers x
--     | x > 1 = x + sumNumbers(x - 1)
--     | x == 1 = 1
sumNumbers 1 = 1
sumNumbers x =  x + sumNumbers(x - 1)

-- Question 6

sumSquares :: Int -> Int
-- sumSquares x
--     | x > 1 = x^2 + sumSquares(x - 1)
--     | x == 1 = 0
sumSquares 1 = 1
sumSquares x = x^2 + sumSquares(x - 1)

-- Queston 7

power :: Int -> Int -> Int
-- power x y 
--      | y > 1 = x * power x (y - 1) 
--      | y == 1 = x
power _ 0 = 1
power 0 _ = 0
power x y = x * power x (y - 1) 

-- Question 8 

-- sumFromTo :: Int -> Int -> Int

sumFromTo x y
    | x > y = error "y must be bigger"
    | x < y = x + sumFromTo (x + 1) y
    | otherwise = y

-- sumFromTo x y = if x > y then  putStrLn 0
-- sumFromTo x y = if x < y then  x + sumFromTo (x + 1) y
-- sumFromTo x y = if x == y then putStrLn x

-- Question 9 

gcd :: Int -> Int -> Int
-- gcd x y | x == y = x
--         | otherwise = gcd (abs (x - y)) (min x y)
-- gcd x y = if x == y then y
gcd 0 y = y
gcd x y = gcd (abs (x - y)) (min x y)

-- Question 10

intSquareRoot :: Int -> Int
intSquareRoot n = findRoot n n

findRoot :: Int -> Int -> Int
-- findRoot n s | s ^ 2 < (n + 1) && s ^ 2 > (n) = s
--              | otherwise = findRoot n (s - 1)
findRoot n s = if s ^ 2 > (n) then findRoot n (s - 1) else s