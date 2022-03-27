-- and recursive
mand :: [Bool] -> Bool
mand [] = False
mand [x] = x
mand (x:xs) = x && mand xs

-- concat recursive
myconcat :: [[a]] -> [a]
myconcat [[x]] = [x]
myconcat (x:xs) = x ++ myconcat(xs)

-- replicate
myrep :: Int -> a -> [a]
myrep 0 x = []
myrep n x = [x] ++ myrep (n-1) x

