{-
recoder
True && True  = True
_ && True  = False
 avec une expression conditionelle
-}

myand3 :: Bool -> Bool -> Bool
myand3 True b = b
myand3 False _ = False


myand4 :: Bool -> Bool -> Bool
myand4 x y = if x then y else False

      
