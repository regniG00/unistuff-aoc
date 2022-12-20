import System.Directory.Internal.Prelude (getArgs)
import Data.Char

main = do args <- getArgs
          file <- readFile (head args)
          putStr (createOutput (getTotalPriorities file)
           (getTotalGroupPrios file))

{-
Erstellt einen String Output für beide Aufgaben
-}
createOutput::Int->Int->String
createOutput a b = "Solution 1: "
 ++ show a ++ "\n\n" ++ "Solution 2: " ++ show b


smallAbc = ['a'..'z']
bigAbc = ['A'..'Z']

{-
Aufgabe 1
-}
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