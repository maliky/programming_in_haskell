-- In a similar manner to the function add, deﬁne a recursive multiplication function mult :: Nat -> Nat -> Nat for the recursive type of natural numbers: Hint: make use of add in your deﬁnition.
data Nat = Zero | Succ Nat  deriving Show

add :: Nat -> Nat -> Nat
add Zero n = n
add n Zero = n
add (Succ m) n = Succ (add m n)

-- mult :: Nat -> Nat -> Nat
-- mult m n = int2nat (nat2int m * nat2int n)

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult n Zero = Zero
mult (Succ m) n = add n (mult m n)


