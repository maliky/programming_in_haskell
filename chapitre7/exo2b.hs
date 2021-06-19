-- exo2 chapitre7 p. 105 alternative
-- http://ix.io/2Ora

test_ :: (Bool -> Bool -> Bool) -> (a -> Bool) -> [a] -> Bool -> Bool
test_ op p xs base_truth_value = foldr test_op base_truth_value xs
  where test_op x b = op (p x) b

any__ :: (a -> Bool) -> [a] -> Bool
any__ p xs = test_ (||) p xs False

all__ :: (a -> Bool) -> [a] -> Bool
all__ p xs = test_ (&&) p xs True
