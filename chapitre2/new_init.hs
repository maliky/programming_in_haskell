-- new init def
-- remove first element
init2 [] = []
init2 ns = reverse (drop 1 (reverse ns))


-- autre def
init3 [] = []
init3 ns =  take (length (ns - 1)) ns
