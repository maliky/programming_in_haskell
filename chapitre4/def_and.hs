-- why the following will not load in ghci?
--     The type signature for ‘myand’ lacks an accompanying binding
myand :: Bool -> Bool -> Bool
True `myand` True = True
True `myand` False  = False
False `myand` True = False
False `myand` False = False 


-- while the loads ok
(&&) :: Bool -> Bool -> Bool
True && True = True
True && False  = False
False && True = False
False && False = False 

