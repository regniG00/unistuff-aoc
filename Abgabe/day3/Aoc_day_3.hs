module Aoc_day_3 (main) where

import System.Environment ( getArgs )
import System.Directory ( doesFileExist )
import System.TimeIt ( timeItT )
import Data.Char


-------------------------------------------------------------------------------
{-
 - Hier bitte die ausgegebene Lösung
 - und die Berechnungsdauer (in Minuten) eintragen:
 
Solution Day 3 Part 1: 7824

Solution Day 3 Part 2: 2798

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
    let solution1 = getTotalPriorities fileContent
        solution2 = getTotalGroupPrios fileContent
    
    putStrLn (solutionString 1 1 solution1)
    putStrLn ""
    putStrLn (solutionString 1 2 solution2)


smallAbc = ['a'..'z']
bigAbc = ['A'..'Z']


getTotalPriorities::String->Int
getTotalPriorities xs = sum (getRucksackPriorites ls)
 where ls = lines xs


getTotalGroupPrios::String->Int
getTotalGroupPrios xs = sum (map getGroupPrio groups)
 where groups = divideIntoGroups (lines xs)

{-
Wandelt eine Liste auf Items in eine Liste der Prioritäten
-}
getRucksackPriorites::[String]->[Int]
getRucksackPriorites [] = []
getRucksackPriorites (x:xs) = (getCharPriority c) : getRucksackPriorites xs
 where c = getRucksackItem x

{-
Gibt das Rucksack Item wieder
-}
getRucksackItem::String->Char
getRucksackItem xs = getDupChar [firstC, secondC]
 where l = div (length xs) 2
       firstC = take l xs
       secondC = drop l xs

{-
Gibt basierend auf dem Rucksack Item die Priorität wieder
-}
getCharPriority::Char->Int
getCharPriority c
 | c `elem` smallAbc = ord c - 96
 | c `elem` bigAbc = ord c - 38
 | otherwise = error "Wrong input"

{-
Gibt einen Char wieder der in allen übergebenen Strings vorkommt
-}
getDupChar::[String]->Char
getDupChar (x:xs) = checkForSameElement x xs
 where checkForSameElement (x:xs) ys = if foldr1 (&&) (map (elem x) ys)
                                       then x 
                                       else checkForSameElement xs ys
getDupChar _ = error "Wrong Input"

{-
Teilt die Stringliste in dreier Listen ein
Input hat 300 Zeilen also es zu keinen Fehler kommen
-}
divideIntoGroups::[String]->[[String]]
divideIntoGroups [] = []
divideIntoGroups (x:y:z:xs) = (x:y:z:[]) : divideIntoGroups xs

getGroupPrio::[String]->Int
getGroupPrio xs = getCharPriority c
 where c = getDupChar xs
