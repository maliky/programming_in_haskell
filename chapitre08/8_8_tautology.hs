-- Extend the tautology checker to support the use of logical disjunction (̣) and equivalence (ô) in propositions.

data Prop = Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Imply Prop Prop
-- Adding the disjunction and equivalence --
  | Or Prop Prop
  | Eqi Prop Prop


p1 :: Prop
p1 = And (Var 'A')  (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A')  (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A')  (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

-- Adding a few properties to test if the Eqi and OR work
p5 :: Prop
p5 = Eqi (Var 'A') (Var 'A')

p6 :: Prop
p6 = Or (Var 'A') (Not (Var 'A'))

p7 :: Prop
p7 = Or (Var 'A') (Var 'A')


type Assoc k v = [(k,v)]
type Subst = Assoc Char Bool

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k==k']

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
-- disjunction and equivalence
eval s (Or p q) = eval s p || eval s q
eval s (Eqi p q) = eval s (Imply p q) && eval s (Imply q p)

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
-- disjunction and equivalence
vars (Eqi p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

type Bit = Int

bools_ :: Int -> [[Bool]]
bools_ n = map (reverse . map conv . make n . int2bin ) range
  where
    range = [0..(2^n) -1]
    make n bs = take n (bs ++ repeat 0)
    conv 0 = False
    conv 1 = True


bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
  where bss = bools (n-1)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools n)
  where vs = rmdups (vars p)
        n =  length vs

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]
