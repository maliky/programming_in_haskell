module Exo6 where
{-
A higher-order function unfold that encapsulates a simple pattern of recursion for producing a list can be deﬁned as follows:
unfold p h t x | p x= []
| otherwise = h x : unfold p h t (t x)
That is, the function unfold p h t produces the empty list if the predicate p is true of the argument value, and otherwise produces a non-empty list by applying the function h to this value to give the head, and the function t to generate another argument that is recursively processed in the same way to produce the tail of the list. For example, the function int2bin can be rewritten more compactly using unfold as follows:

int2bin = unfold (== 0) (‘mod‘ 2) (‘div‘ 2)

Redeﬁne the functions chop8, map f and iterate f using unfold
-}

-- unfold :: (a -> Bool) -> (a -> a) -> (a -> a) -> a -> [a]
-- why no type definission here?
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)


int2bin_ :: Int -> [Int]
int2bin_ = unfold (== 0) (`mod` 2) (`div` 2)

-- chop8_
chop8_ :: [a] -> [[a]]
chop8_ = unfold (null) (take 8) (drop 8)


-- map f
map_ :: (a -> b) -> [a] -> [b]
map_ f xs = unfold (null) (f . head) (tail) xs
  

-- iterate f
iterate_ :: (a -> a) -> a -> [a]
iterate_ f x = unfold (null) (id) (f) (x)


-- ghci error
Prelude> :l exo6.hs
[1 of 1] Compiling Exo6             ( exo6.hs, interpreted )

exo6.hs:36:36: error:
    • Couldn't match type ‘a’ with ‘t0 a0’
      ‘a’ is a rigid type variable bound by
        the type signature for:
          iterate_ :: forall a. (a -> a) -> a -> [a]
        at exo6.hs:35:1-32
      Expected type: t0 a0 -> t0 a0     
        Actual type: a -> a
    • In the third argument of ‘unfold’, namely ‘(f)’
      In the expression: unfold (null) (id) (f) (x)
      In an equation for ‘iterate_’:
          iterate_ f x = unfold (null) (id) (f) (x)
    • Relevant bindings include
        x :: a (bound at exo6.hs:36:12)
        f :: a -> a (bound at exo6.hs:36:10)
        iterate_ :: (a -> a) -> a -> [a] (bound at exo6.hs:36:1)
   |
36 | iterate_ f x = unfold (null) (id) (f) (x)
   |                                    ^
Failed, no modules loaded.

