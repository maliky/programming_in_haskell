-- j'essaye de voir la différence entre renvois simple et return IO
test :: String -> Int
test "a" = 1
test _ = 0

test_ :: String -> IO Int
test_ "a" = return 1
test_ _ = return 0

gen :: String -> [Int]
gen x = [test x | n <- [1..5]]


gen_ :: String -> [IO Int]
gen_ x = [test_ x | n <- [1..5]]
