--
-- MATHFUN
-- Add your student number
-- 934378

import Data.List
import qualified Data.Text as T
import System.IO  
import System.Directory  
import System.Exit
-- Types (define City type here)
--
import Text.Parsec


data City = City {  name :: String,
                    locNorth :: Int,
                    locEast :: Int,
                    listOfPopulation :: [Int]
                    }deriving (Show)

testData :: [City]
testData = [     
    City "Amsterdam"    52   5    [1158, 1149, 1140, 1132],
    City "Athens"       38  23    [3153, 3153, 3154, 3156],
    City "Berlin"       53  13    [3567, 3562, 3557, 3552],
    City "Brussels"     51   4    [2096, 2081, 2065, 2050],
    City "Bucharest"    44  26    [1794, 1803, 1812, 1821],
    City "London"       52   0    [9426, 9304, 9177, 9046],
    City "Madrid"       40   4    [6669, 6618, 6559, 6497],
    City "Paris"        49   2    [11079, 11017, 10958, 10901],
    City "Rome"         42  13    [4278, 4257, 4234, 4210],
    City "Sofia"        43  23    [1284, 1281, 1277, 1272],
    City "Vienna"       48  16    [1945, 1930, 1915, 1901],
    City "Warsaw"       52  21    [1790, 1783, 1776, 1768] 
    ]

--
--  Your functional code goes here
--
-- Core Functionality i
listOfNames :: [City] -> [String]
listOfNames [] = error "Empty data set"
listOfNames (x:xs) 
    | length xs == 0 = name x : [] 
    | otherwise = name x : listOfNames xs

listOfNamesPop :: [City] -> [(String , [Int])]
listOfNamesPop (x:xs) 
    | length xs == 0 = (name x , listOfPopulation x) : [] 
    | otherwise = (name x , listOfPopulation x) : listOfNamesPop xs

-- Core Functionality ii
checkName:: String -> [(String , [Int])] -> [Int]
checkName name (x:xs)
    | name == fst x = snd x
    | xs == [] = error "City not found"
    | otherwise = checkName name xs

cityNamePop:: [City] -> String -> Int -> String
cityNamePop cities cityName year = show (fromIntegral(checkName cityName (listOfNamesPop cities) !! year) / 1000 ) ++ "m"

-- Core Functionality iii
printTableLine:: [City] -> IO()
printTableLine (city:cities) 
    | length cities == 0 = putStrLn(name city ++ "   " ++ (show (locNorth city)) ++ "   " ++ (show (locEast city)) ++ "   " ++ cityNamePop (city:cities) (name city) 0 ++ "   " ++ cityNamePop (city:cities) (name city) 1)
    | otherwise = putStr(name city ++ "   " ++ (show (locNorth city)) ++ "   " ++ (show (locEast city)) ++ "   " ++ cityNamePop (city:cities) (name city) 0 ++ "   " ++ cityNamePop (city:cities) (name city) 1 ++ "\n") >> printTableLine cities

printTable:: [City] -> IO()
printTable city = putStr("Name" ++ "   " ++ "Loc N " ++ "   " ++ "Loc E " ++ "   " ++ "This Years Pop" ++ "   " ++ "Last Years Pop" ++"\n") >> printTableLine city


-- Core Functionality iv

updatePopListIndvCity:: Int -> City -> City
updatePopListIndvCity newPop (City name locN locE pop) =  City name locN locE (newPop: pop)

updatePopOfData:: [Int] -> [City] -> [City]
updatePopOfData [] [] = []
updatePopOfData (pop:pops) (city:cities) =  updatePopListIndvCity pop city: updatePopOfData pops cities

-- Core Functionality v

addNewCity:: [City] -> String -> Int -> Int -> [Int] -> [City]
addNewCity [] cityName cityLocN cityLocE cityPop = City cityName cityLocN cityLocE cityPop : []
addNewCity (city:cities) cityName cityLocN cityLocE cityPop
    | cityName > name city  = city:addNewCity cities cityName cityLocN cityLocE cityPop
    | otherwise = City cityName cityLocN cityLocE cityPop : city : cities


-- Core Functionality vi

percPopFigures:: [City] -> String -> String
percPopFigures [] _ = error "City not Found"
percPopFigures (city:cities) cityName 
    | name city == cityName = "First year: " ++ show((((fromIntegral(listOfPopulation city !! 1) - fromIntegral(listOfPopulation city !! 0)) / fromIntegral(listOfPopulation city !! 0))) * 100) ++ "% | Second year: " ++ show((((fromIntegral(listOfPopulation city !! 2) - fromIntegral(listOfPopulation city !! 1)) / fromIntegral(listOfPopulation city !! 1))) * 100) ++ "%"
    | otherwise = percPopFigures cities cityName
 

-- Core Functionality vii

citySep :: Int -> Int -> Int -> Int -> Float
citySep _north1 _east1 _north2 _east2 = acos ((cos east1 * cos north1 * cos east2 * cos north2) + (sin east1 * cos north1 * sin east2 * cos north2) + (sin north1 * sin north2))
    where
        north1 = fromIntegral _north1 * pi / 180
        north2 = fromIntegral _north2 * pi / 180
        east1 = fromIntegral _east1 * pi / 180
        east2 = fromIntegral _east2 * pi / 180

nearCity :: [City] -> Int -> Int -> City
nearCity [] _ _ = error "No city"
nearCity cities north east = minimumBy (\a b -> compare (citySep (locNorth a) (locEast a) north east) (citySep (locNorth b) (locEast b) north east)) cities

filterPopOver :: [City] -> Int -> [City]
filterPopOver cities pop = [c | c <- cities, (head.listOfPopulation) c > pop]

--  Demo
--

demo :: Int -> IO ()
demo 1 = print(listOfNames testData)
demo 2 = putStrLn(cityNamePop testData "Madrid" 2)
demo 3 = printTable testData
demo 4 = printTable ((updatePopOfData [1200,3200,3600,2100,1800,9500,6700,11100,4300,1300,2000,1800] testData))
demo 5 = printTable (addNewCity testData "Prague" 50 14 [1312, 1306, 1299, 1292])
demo 6 = putStrLn(percPopFigures testData "London")
demo 7 = putStrLn(name (nearCity (filterPopOver testData 2000) 54 6)) 
demo 8 = clearScreen >> mapWrite(cityNorthSort testData)


--
-- Screen Utilities (use these to do the population map)
--

type ScreenPosition = (Int,Int)

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
    goTo position
    putStr text
 

--
-- Your population map code goes here
--

mapWrite:: [City] -> IO()
mapWrite [] = goTo(0,80)
mapWrite (city:cities)
    | length cities == 0 = writeAt (((locNorth city)), ((locEast city))) ("+ " ++ name city ++ " | "++ (cityNamePop testData (name city) 0) ++ "\n")
    | locNorth city == locNorth (head cities) = writeNorth (city:cities)
    | otherwise = writeAt (((locNorth city)), ((locEast city))) ("+ " ++ name city ++ " | "++ (cityNamePop testData (name city) 0) ++ "\n") >> mapWrite cities

writeNorth:: [City] -> IO()
writeNorth (city:cities)
    | locNorth city == locNorth (head cities) && (locEast city - locEast (head cities)) > 15 = writeAt (((locNorth city)), ((locEast city) + 15)) ("+ " ++ name city ++ " | "++ (cityNamePop testData (name city) 0)) >> writeNorth cities
    | locNorth city == locNorth (head cities) && (locEast city - locEast (head cities)) <= 15 = writeAt (((locNorth city)), ((locEast city)- 15)) (name city ++ " | "++ (cityNamePop testData (name city) 0) ++ " +") >> writeNorth cities
    | locNorth city /= locNorth (head cities) && (locEast city - locEast (head cities)) > 15 = writeAt (((locNorth city)), ((locEast city)+ 15)) ("+ " ++ name city ++ " | "++ (cityNamePop testData (name city) 0) ) >> mapWrite cities
    | otherwise = writeAt (((locNorth city)), ((locEast city)- 15)) (name city ++ " | "++ (cityNamePop testData (name city) 0) ++ " +" ) >> mapWrite cities

cityNorthSort:: [City] -> [City]
cityNorthSort [] = []
cityNorthSort listofCities =  sortOn locNorth listofCities



-- city

--
-- Your user interface (and loading/saving) code goes here
--
main:: IO() 
main = do 
    fileDataString <- openFileData "cities.txt"
    print(fileDataString)
    interfaceMenu
    


openFileData:: String -> IO()
openFileData fileName = do
    openFile <- openFile fileName ReadWriteMode
    contents <- hGetContents openFile 
    let cityToDo = lines contents   
    printLines cityToDo
    hClose openFile

printLines:: [String] -> IO()
printLines (line:lines) 
    | length lines == 0 = putStrLn (line)
    | otherwise = putStrLn (line) >> printLines lines

interfaceMenu::IO() 
interfaceMenu = putStrLn("Menu - type below to achieve \n" ++ "------\n" ++ "demo 1 - List of Cities \n" ++ "demo 2 - Give a city name and years ago (ie London 1) will return population\n" ++ "demo 3 - Table of Cities\n" ++ "demo 4 - Update the list of population must provide an integer list\n" ++ "demo 5 - Add a new city - Must provide Name LocN LocE and a List of population\n" ++ "demo 6 - Percentage Pop Growth figures must provide list of data and city\n" ++ "demo 7 - Closest City need to prove a population number and locations\n" ++ "demo 8 - Location map\n" ++ "Exit")
   

exit::IO()
exit = do
    openFile <- openFile "cities.txt" ReadWriteMode
    contents <- hGetContents openFile 
    writeFile "cities.txt" (contents)
    exitSuccess
