module GetNumber where
import Chapitre10
import Data.Char -- for digitToInt

-- vérifie que le nombre passé comme String est fait des 10 chiffres arabes
isNumber_f :: String -> Bool
isNumber_f = foldl (\x y ->  x && isDigit y) True

-- Convertis le nombre passé en String en base 10
numberToInt_f ::  String -> Int
numberToInt_f = foldl (\x y -> x * 10 + digitToInt y) 0


getNumber :: IO Int
getNumber = do putStr "Entrer un nombre : "
               n <- getLine_  -- Interaction récupération état du monde
               if isNumber_f n then
                  return (numberToInt_f n) -- need to return the world that was changed
               else
                  do putStrLn "Error : Invalid number.  Recommencez."
                     getNumber
