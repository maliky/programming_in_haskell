-- incorrect
-- N = a 'div' length xs
--   where
--     a = 10
--    xs = [1,2,3,4,5]

-- n = a `div` (length xs)
-- problème avec l'indentation and scope
-- Variable not in scope: xs :: [a0]
n = a `div` length xs
  where
    a = 10
    xs = [1,2,3,4,5]
-- Solition utiliser :load et non script dans ghci

-- problème avec l'indentation
-- n = a `div` (length xs) where {a = 10; xs = [1,2,3,4,5]};
