{-
recoder
True && True  = True
_ && True  = False
 avec une expression conditionelle
-}

myand :: Bool -> Bool -> Bool
myand True True = True
myand _ _ = False


myand2 :: Bool -> Bool -> Bool
myand2 x y =
  if x == y then
    if x then True
    else False
   else False
      
