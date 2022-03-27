import Chapitre10
import Data.Char -- for digitToInt
import GetNumber -- getNumber, getDigit


-- fonction intermédiare : 
inter_adder :: Int -> Int -> IO Int
inter_adder total 0 = return total
inter_adder total len_reste = do next_digit <- getDigit
                                 inter_adder (total + next_digit) (len_reste - 1)


-- Fonction qui demande d'entrer un nombre de terme
-- puis les termes et les sommes.
adder :: IO String
adder = do d <- getNumber
           total <- inter_adder 0 d
           return ("Le total des "++ (show d) ++ " chiffres est : " ++ (show total))


