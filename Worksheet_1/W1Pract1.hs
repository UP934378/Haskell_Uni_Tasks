-- Worksheet 1 - Introduction to Functional Programming
-- 934378 Tom Angus

-- Q1 - Multiply by 10

timesTen :: Int -> Int
timesTen x = 10 * x

-- Q2 - Sum of three integers

sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

-- Q3 - Radius of a Circle

areaOfCircle :: Float -> Float
areaOfCircle x = pi * x^2

-- Q4 - Volume of a Cylinder

volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder x y = (areaOfCircle x) * y

-- Q5 - Distance between points

distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt(((y1 - y2)^2) + ((x1 - x2)^2))

-- Q6 - Check Difference
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z = x /= y && x /= z && z /= y

-- Q7 - Divisable by another

divisibleBy :: Int -> Int -> Bool
divisibleBy x y = mod x y == 0

-- Q8 - Is an even number

isEven :: Int -> Bool
isEven x = divisibleBy x 2

-- Q9 - Average of Three ints

averageThree :: Int -> Int -> Int -> Float
averageThree x y z = fromIntegral (x + y + z) / 3

-- Q10 - absolute of Integer

absolute :: Int -> Int
-- absolute x = if x < 0 then x + ((-(x)) * 2) else x
absolute x
    | x<0 = -x
    | otherwise = x
