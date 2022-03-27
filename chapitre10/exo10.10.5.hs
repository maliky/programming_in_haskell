import Chapitre10

getNumber_ :: String -> IO Int
getNumber_ prompt = do
  putStr prompt
  n <- getLine -- Interaction récupération état du monde
  return (read n :: Int)

adder_ :: IO String
adder_ = do
  t <- getNumber_ "Entrer la taille de la séquence. "
  seq_ <- sequence [getNumber_ "Entrer un chiffre : " | _ <- [1 .. t]]
  return ("Le total de " ++ show seq_ ++ " est : " ++ show (sum seq_))
