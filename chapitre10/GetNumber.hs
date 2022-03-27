module GetNumber where
import Chapitre10
import Data.Char -- for digitToInt

-- vérifie que le nombre passé comme String est fait des 10 chiffres arabes
isNumber_f :: String -> Bool
isNumber_f = foldl (\x y ->  x && isDigit y) True

-- Convertis le nombre passé en String en base 10
numberToInt_f ::  String -> Int
numberToInt_f = foldl (\x y -> x * 10 + digitToInt y) 0


-- Demande et retourn un entier
getNumber :: IO Int
getNumber = do putStr "Entrer un nombre : "
               n <- getLine_  -- Interaction récupération état du monde
               if isNumber_f n then
                  return (numberToInt_f n) -- need to return the world that was changed
               else
                  do putStrLn "Error : Invalid number.  Recommencez."
                     getNumber

isDigit_S :: String -> Bool
-- Teste si le String est un chifre en base 10
isDigit_S [x] = isDigit x
isDigit_S _ = False

-- Demande et retourne un chiffre en base 10
getDigit :: IO Int
getDigit = do putStr "Saisir un chiffre (0-9) : "
              n <- getLine_  -- Interaction récupération état du monde
              if isDigit_S n then
                 return (numberToInt_f n) -- return the digit and the world that changed (because of Input)
              else
                 do putStrLn "Error : Chiffre invalide.  Recommencez."
                    getDigit

-- getNumber plus simple
getNumber_ ::  IO Int
getNumber = do putStr "Entrer un nombre : "
               n <- getLine_  -- Interaction récupération état du monde
               return (read n :: Int)
