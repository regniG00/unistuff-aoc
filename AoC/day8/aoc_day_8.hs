import System.Directory.Internal.Prelude (getArgs)
import Data.List
import Data.Array

main = do args <- getArgs
          file <- readFile (head args)
          putStr (createOutput (task1 file) 0)

{-
Erstellt einen String Output für beide Aufgaben
-}
createOutput::Int->Int->String
createOutput a b = "Solution 1: "
 ++ show a ++ "\n\n" ++ "Solution 2: " ++ show b

task1::String->Int
task1 xs = length (head (lines xs))


array1 = array ((0,0),(10,12)) [((x,y),x*y) | x<-[0..10], y<-[0..12]] 
array2 = listArray (4, 8) [2, 4, 6, 8, 10]