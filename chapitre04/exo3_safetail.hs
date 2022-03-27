{-
Définir la fonction safetail qui renvois [] à la place de produire une erreur en utilisant:
- une expression conditionel
- des gardes (garded equations)
- patter matching
-}


safetail :: [a] -> [a] 
safetail xs = if length xs == 0 then [] else tail xs
-- safetail xs = if null then [] else tail xs

safetail2 :: [a] -> [a]
safetail2 xs | length xs ==0  = []
             | otherwise = tail xs

safetail3 :: [a] -> [a]
safetail3 []  = []
safetail3 xs = tail xs

-- safetail3 (_:xs) =  xs
