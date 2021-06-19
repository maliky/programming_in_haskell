{-
Comme pour && in section.4. montre que l'opérateur de disjonction peut être définie de 4 façons différentes
-}

dj :: Bool -> Bool -> Bool
dj True True = True
dj True False = True
dj False True = True
dj False False = False

dj2 :: Bool -> Bool -> Bool
dj2 False False = False
_ || _ = True 

dj3 :: Bool -> Bool -> Bool
dj3 False b = b
_ || _ = True 

dj4 :: Bool -> Bool -> Bool
dj4 b c | b == c = b
        | otherwise = True
