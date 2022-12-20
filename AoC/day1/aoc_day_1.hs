{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import System.Directory.Internal.Prelude (getArgs)
import Data.List

{-
Maximale Kalorien: 68775

Summe der 3 größten Kalorien: 202585
-}

main = do args <- getArgs
          file <- readFile (head args)
          putStr (createOutput (getMostCalories file) (getMost3Calories file))

type Elve=[String]

createOutput::Int->Int->String
createOutput a b = "Solution 1: "
 ++ show a ++ "\n\n" ++ "Solution 2: " ++ show b

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