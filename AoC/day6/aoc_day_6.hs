import System.Directory.Internal.Prelude (getArgs)
import Data.List

main = do args <- getArgs
          file <- readFile (head args)
          putStr (createOutput (task1 file) (task2 file))

{-
Erstellt einen String Output für beide Aufgaben
-}
createOutput::Int->Int->String
createOutput a b = "Solution 1: "
 ++ show a ++ "\n\n" ++ "Solution 2: " ++ show b

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