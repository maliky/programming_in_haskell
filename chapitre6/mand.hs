mand :: [Bool] -> Bool
mand [] = False
mand [x] = x
mand (x:xs) = x && mand xs


myconcat :: [[a]] -> [a]
myconcat [[x]] = [x]
myconcat (x:xs) = x ++ myconcat(xs)
