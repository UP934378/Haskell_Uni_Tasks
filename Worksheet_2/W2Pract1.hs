-- Tom Angus
-- UP934378


-- Q1 - New Absolute

absolute :: Int -> Int
absolute x
    | x<0 = -x
    | otherwise = x

-- Q2 - Postive or negative

sign :: Int -> Int
sign x
    | x<0 = -1
    | x>0 = 1
    |otherwise = 0

-- Q3 - How many equal

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z 
    | x == y && x == z && z == y = 3 
    | x == y = 2
    | x == z = 2
    | y == z = 2
    | otherwise = 0
    
-- Q4 - Sum of Diagonal Lengths

sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths x y z = pythago x + pythago y + pythago y
    where
    pythago a = sqrt (a ^ 2 * 2)

-- Q5 - TaxiFare

taxiFare :: Int -> Float
taxiFare x = 2.2 + kilo + additKilo
    where
        kilo 
            | x <= 10 = fromIntegral(x) * 0.5
            | otherwise = fromIntegral 5
        additKilo 
            | x <= 10  = fromIntegral 0 
            | otherwise = fromIntegral((x - 10)) * 0.3

-- Q6 - How many Above Average

howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage x y z 
    |x > averageInt x y z && y > averageInt x y z = 2
    |z > averageInt x y z && y > averageInt x y z = 2
    |x > averageInt x y z && z > averageInt x y z = 2
    |x > averageInt x y z = 1
    |z > averageInt x y z = 1
    |y > averageInt x y z = 1
    |otherwise = 0

averageInt:: Int -> Int -> Int -> Int
averageInt x y z = (x + y + z) `div` 3

-- Q7 - Validate date

validDate :: Int -> Int -> Bool
validDate x y  
    | lenMonth y == 0 = False
    | lenMonth y < x = False 
    | otherwise = True


lenMonth:: Int -> Int
lenMonth x 
    | x == 2  = 28
    | x `elem` [1,3,5,7,8,10,12] = 31
    | x `elem` [4,6,9,11] = 30
    | otherwise = 0


-- Q8 - Days in month

daysInMonth :: Int -> Int -> Int
daysInMonth x y
    |lenMonth x == 0 = 0
    |lenMonth x == 28 && rem y 4 == 0 = 29 
    |lenMonth x == 28 = 28
    |otherwise  = lenMonth x

-- Written Questions

-- Written exercises
-- 1. For your sumThree function from worksheet 1, give calculations that evaluate the following expressions:
-- sumThree x y z = x + y + z
-- • sumThree 3 5 7
-- - 3 + 5 + 7 = 15

-- • sumThree 8 (1 + 3) 2
-- - 8 + (1 + 3) + 2
-- - 8 + 4 + 2 = 14


-- 2. For your threeDifferent function from worksheet 1, give calculations that evaluate
-- the following expressions:
-- threeDifferent x y z = x /= y && x /= z && z /= y
-- • threeDifferent 1 4 2
-- - 1 = True 
-- - 4 = True
-- - 7 = True

-- • threeDifferent 1 7 7
-- - 1 = True 
-- - 7 = False
-- - 7 = False

-- 3. For your howManyEqual function from this worksheet, give calculations that evaluate
-- the following expressions:
-- howManyEqual x y z 
--     | x == y && x == z && z == y = 3 
--     | x == y = 2
--     | x == z = 2
--     | y == z = 2
--     | otherwise = 0
-- • howManyEqual 3 5 2
-- - 
-- • howManyEqual 5 2 5

