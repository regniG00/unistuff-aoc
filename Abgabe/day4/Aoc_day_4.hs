module Aoc_day_4 (main) where

import System.Environment ( getArgs )
import System.Directory ( doesFileExist )
import System.TimeIt ( timeItT )


-------------------------------------------------------------------------------
{-
 - Hier bitte die ausgegebene Lösung
 - und die Berechnungsdauer (in Minuten) eintragen:
 
Solution Day 4 Part 1: 539

Solution Day 4 Part 2: 871

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
    let solution1 = task1 fileContent
        solution2 = task2 fileContent
    
    putStrLn (solutionString 1 1 solution1)
    putStrLn ""
    putStrLn (solutionString 1 2 solution2)

{-
Funktion für Aufgabe 1

Lösung: 539
-}
task1::String->Int
task1 xs = countSecs allIntPairs includingSection
 where allIntPairs = getAllIntPairs (lines xs)

{-
Funktion für Aufgabe 2

Lösung: 871
-}
task2::String->Int
task2 xs = countSecs allIntPairs overlappingSection
 where allIntPairs = getAllIntPairs (lines xs)
    
{-
Macht aus einer String liste mit dem String Format
xx-xx,xx-xx ein Liste aus Intpaaren
-}
getAllIntPairs::[String]->[[[Int]]]
getAllIntPairs (x:xs) = map getPairs xs

{-
Macht aus einem String mit dem Format xx-xx,xx-xx
ein Intpaar
-}
getPairs::String->[[Int]]
getPairs xs = temp (divideListByElement ',' xs)
 where temp []     = []
       temp (x:xs) = (getIntPair x) : temp xs 

{-
Wandelt einen xx-xx String in eine Int Liste um 
-}
getIntPair::String->[Int]
getIntPair xs = firstNumber:secondNumber:[]
 where firstNumber  = read (head ls)
       secondNumber = read (last ls)
       ls = divideListByElement '-' xs

{-
Prüft ob eine Section die andere Mitbeinhaltet   
-}
includingSection::[[Int]]->Bool
includingSection [x,y] = (x1 <= y1 && x2 >= y2) ||
                           (x1 >= y1 && x2 <= y2)
                  where x1 = head x
                        x2 = last x
                        y1 = head y
                        y2 = last y
includingSection _ = error "Wrong input"

{-
Prüft ob sich Sections nach Aufgabe 2 überlappen   
-}
overlappingSection::[[Int]]->Bool
overlappingSection [x,y] = if x1 <= y1
                           then y1 <= x2
                           else x1 <= y2
                  where x1 = head x
                        x2 = last x
                        y1 = head y
                        y2 = last y
overlappingSection _ = error "Wrong input"

{-
Zählt die Sections die Überlappen
-}
countSecs::[[[Int]]]->([[Int]]->Bool)->Int
countSecs [] f = 0
countSecs (x:xs) f = if f x
                              then 1 + countSecs xs f
                              else countSecs xs f

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
