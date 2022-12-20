import System.Directory.Internal.Prelude (getArgs)
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