-- deriving Show

data Maybe_ a = Nothing_ | Just_ a deriving Show

safediv :: Int -> Int -> Maybe_ Int
safediv _ 0 = Nothing_
safediv m n = Just_ (m `div` n)

safehead :: [a] -> Maybe_ a
safehead []= Nothing_
safehead xs = Just_   (head xs)
