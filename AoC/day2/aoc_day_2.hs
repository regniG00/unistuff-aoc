import System.Directory.Internal.Prelude (getArgs)

{-
Lösung Aufgabe 1: 11449

Lösung Aufgabe 2: 13187
-}

main = do args <- getArgs
          file <- readFile (head args)
          putStr (createOutput (getTotalScore file) (getTotalScore2 file))

{-
Erstellt einen String Output für beide Aufgaben
-}
createOutput::Int->Int->String
createOutput a b = "Solution 1: "
 ++ show a ++ "\n\n" ++ "Solution 2: " ++ show b

{-
Aufgabe 1
-}
getTotalScore::String->Int
getTotalScore xs = totalScore (lines xs)

{-
Aufgabe 2
-}
getTotalScore2::String->Int
getTotalScore2 xs = totalScore2 (lines xs)

{-
Schere Stein Papier zeichen
-}
data Shape = Rock | Paper | Scissors
 deriving (Show, Ord, Eq, Enum, Bounded)

{-
Pred Funktion die beim minBound den maxBound zurück gibt
-}
cyclePred::(Eq a, Enum a, Bounded a)=> a -> a
cyclePred a
 | a == minBound = maxBound
 | otherwise = pred a
{-
Succ Funktion die beim maxBound den minBound zurück gibt
-}
cycleSucc::(Eq a, Enum a, Bounded a)=> a -> a
cycleSucc a 
 | a == maxBound = minBound
 | otherwise = succ a

{-
Funktion die aus den jeweiligen Buchstaben ein Zeichen wiedergibt
-}
getShape::Char->Shape
getShape c 
 | c=='A' || c=='X' = Rock
 | c=='B' || c=='Y' = Paper
 | c=='C' || c=='Z' = Scissors
 | otherwise = error "Wrong input"

{-
Gibt 6 Punkte bei einem Gewinn wieder
3 Punkte bei Unentschieden und
0 wenn man verloren hat

Dabei ist der zweite "Shape" vom Spieler (Also X Y oder Z)
-}
matchResult::Shape->Shape->Int
matchResult a b
 | a == b = 3
 | a == cyclePred b = 6
 | a == cycleSucc b = 0

{-
Gibt einen Wert je nach dem Zeichen wieder
-}
shapeScore::Shape->Int
shapeScore Rock     = 1
shapeScore Paper    = 2
shapeScore Scissors = 3

{-
Gibt die Gesamtpunkte einer Runde wieder
Also je nachdem ob man Gewonnen hat oder nicht und welches 
Zeichen man benutzt hat 
-}
calcRound::String->Int
calcRound xs = (matchResult shapeA shapeB) + shapeScore shapeB
         where shapeA = getShape (head xs)
               shapeB = getShape (last xs)

{-
Berechnet die Gesamtpunktzahl des Inputs
-}
totalScore::[String]->Int
totalScore xs = foldr ((+) . calcRound) 0 xs

{-
Gibt as Zeichen zurück je nachdem welches Ergebnis erwünscht ist
-}
getDesiredShape::Shape->Char->Shape
getDesiredShape s 'X' = cyclePred s
getDesiredShape s 'Y' = s
getDesiredShape s 'Z' = cycleSucc s

{-
Berechnet die Gesamtpunktzahl des Inputs nach Aufgabe 2
-}
totalScore2::[String]->Int
totalScore2 xs = foldr ((+) . calcRound2) 0 xs

calcRound2::String->Int
calcRound2 xs = (matchResult shapeA shapeB) + shapeScore shapeB
         where shapeA = getShape (head xs)
               shapeB = getDesiredShape shapeA (last xs)