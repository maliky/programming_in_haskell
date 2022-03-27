{--
Complete the following instance declarations:
instance Eq a => Eq (Maybe a) where
...
instance Eq a => Eq [a] where
--}

instance Eq a => Eq (Maybe a) where
  (==), (/=) :: (Maybe a) -> (Maybe a) -> Bool

  x /= y = not (x == y)

instance Eq a => Eq [a] where
