module Aoc_day_1 (main) where

import System.Environment ( getArgs )
import System.Directory ( doesFileExist )
import System.TimeIt ( timeItT )


-------------------------------------------------------------------------------
{-
 - Hier bitte die ausgegebene Lösung
 - und die Berechnungsdauer (in Minuten) eintragen:
 
Solution Day 1 Part 1: 68775

Solution Day 1 Part 2: 202585

Berechnungsdauer: 0 Minuten

-}
-------------------------------------------------------------------------------


-- | Das ist das Format, das für die Lösung ausgegeben werden soll.
solutionString :: Show a => Int -> Int -> a -> String
solutionString day part answer = "Solution Day " ++ show day ++ " Part "
                               ++ show part ++ ": " ++ show answer

-- | Das ist das Format, das für die Berechnungsdauer ausgegeben werden soll.
timeString :: Double -> String
timeString timeInS = "Berechnungsdauer: " ++ show timeInM ++ " Minuten"
  where
    timeInM = round $ timeInS / 60 :: Int

{-|
 - Wird bei fehlendem Kommandozeilenargument oder einem solchen,
 - der eine nicht lesbare oder nicht vorhandene Datei benennt, aufgerufen.
 -}
noFileFound :: String -> IO ()
noFileFound str = putStrLn $ "Dateiname existiert nicht: " ++ str


-- | Das Hauptprogramm, das bei Ausführung gestartet wird.
main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> noFileFound "" -- Kein Argument übergeben.
        (fileName:_) -> do
            exists <- doesFileExist fileName
            if exists
                then do
                    content <- readFile fileName
                    {-
                     - TODO: Falls Funktionsname und/oder -signatur
                     - von doStuff verändert werden,
                     - muss das entsprechend hier angepasst werden.
                     -}
                    (time, _) <- timeItT $ doStuff content
                    putStrLn ""
                    putStrLn $ timeString time
                else noFileFound fileName -- Datei existiert nicht.



{-|
 - Diese Funktion bekommt den Inhalt der eingelesenen Datei übergeben.
 - Was sie dann damit macht, ist ganz Ihnen überlassen, bzw. der Aufgabe,
 - die es zu erfüllen gilt. Hier ist nur ein kleines, ausführbares Beispiel.
 -}
-- TODO:
doStuff :: String -> IO ()
doStuff fileContent = do
    let solution1 = getMostCalories fileContent
        solution2 = getMost3Calories fileContent
    
    putStrLn (solutionString 1 1 solution1)
    putStrLn ""
    putStrLn (solutionString 1 2 solution2)
  
    
{-
Haupfunktion für Teil 1
Nimmt den Inputstring und gibt den Elf mit den meisten Kalorien zurück
-}
getMostCalories::String->Int
getMostCalories xs = maximum calList
 where calList = getCalList elves
       elves = createElves (lines xs)

{-
Haupfunktion für Teil 2
-}
getMost3Calories::String->Int
getMost3Calories xs = sum (getFirst3 calList)
 where calList = getCalList elves
       elves = createElves (lines xs)

type Elve=[String]

{-
Funktion die eine Liste an jeder Stelle aufteilt bei der 
das gegebene Elment vorkommt
-}
divideListByElement::(Eq a)=>a->[a]->[[a]]
divideListByElement _ [] = []
divideListByElement e xs = divide xs
    where divide [] = [[]]
          divide (x:xs) | e == x = [] : divide xs
                        | otherwise = (x:y):ys
                        where (y:ys) = divide xs

{-
Erstellt eine Liste für jeden einzelnen Elfen
-}
createElves::[String]->[Elve]
createElves xs = divideListByElement "" xs

{-
Gibt die Gesamtkalorien des Elfen wieder
-}
getCalSum::Elve->Int
getCalSum xs = foldr ((+) . read) 0 xs

{-
Macht aus einer Liste von Elfen eine Liste mit den Gesamtkalorien der Elfen
-}
getCalList::[Elve]->[Int]
getCalList xs = map getCalSum xs

{-
Sortiert eine Liste und gibt die ersten Drei Elemente zurück
-}
getFirst3::[Int]->[Int]
getFirst3 xs = take 3 (qsort xs)

{-
Quicksort Algorithmus von
https://wiki.c2.com/?QuickSortInHaskell

So umgeändert, dass von groß nach klein sortiert wird
-}
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort l@(x:xs) = qsort large ++ mid ++ qsort small
  where
    small = [y | y<-xs, y<x]
    mid   = [y | y<-l, y==x]
    large = [y | y<-xs, y>x]
