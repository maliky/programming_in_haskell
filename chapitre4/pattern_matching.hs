not' :: Bool -> Bool
not' False = True
not' True = False


-- using a function as an operator
myand :: Bool -> Bool -> Bool
True `myand` True = True
True `myand` False  = False
False `myand` True = False
False `myand` False = False 


-- redefinition of the operator
(&&) :: Bool -> Bool -> Bool
True && True = True
True && False  = False
False && True = False
False && False = False 


{-
_ is the wildcard
-}

-- simplification du mand
myand' :: Bool -> Bool  -> Bool
True `myand'` True  = True 
_ `myand'` _ = False 

myand'' :: Bool -> Bool  -> Bool
True `myand''` b = b
False `myand''` _ = False 


-- en utilisant les variable de classe
myand3 :: Bool -> Bool  -> Bool
myand3 b  c | b == c = b
            | otherwise = False 
