module Aoc_day_4 (main) where

import System.Environment ( getArgs )
import System.Directory ( doesFileExist )
import System.TimeIt ( timeItT )
import Data.List


-------------------------------------------------------------------------------
{-
 - Hier bitte die ausgegebene Lösung
 - und die Berechnungsdauer (in Minuten) eintragen:
 
Solution Day 6 Part 1: 1100

Solution Day 6 Part 2: 2421

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

Lösung: 1100
-}
task1::String->Int
task1 xs = getFirstMakerIndex xs 4 + 4

{-
Funktion für Aufgabe 2

Lösung: 2421
-}
task2::String->Int
task2 xs = getFirstMakerIndex xs 14 + 14

{-
Gibt den Index wieder vom ersten Char dessen darauffolgende Chars einzigartig
sind

Der Int gibt an wie viele Char einzigartig seien sollen
-}
getFirstMakerIndex::String->Int->Int
getFirstMakerIndex xs i = if checkIfDistinc (take i xs)
                        then 0
                        else 1 + getFirstMakerIndex (tail xs) i

{-
Prüft ob alle Char im String einzigartig sind
-}
checkIfDistinc::String->Bool
checkIfDistinc [] = True
checkIfDistinc (x:xs) = if x `elem` xs then False else checkIfDistinc xs
