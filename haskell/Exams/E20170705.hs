module E20170705 where

-- Consider the following binary tree data structure:
data Tree a = Nil | Leaf a | Branch (Tree a) (Tree a) deriving (Show, Eq)
-- 1) Define a tcompose operation, 
--    which takes a function f and two trees, t1 and t2, 
--    and returns a tree with the same structure as t1,
--    but with leaves replaced by subtrees 
--    having the same structure of t2: 
--    each leaf is obtained by applying f 
--    to the value stored in the
--    previous leaf, and the corresponding value in t2.
-- E.g. t1 = Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3), 
--      t2 = Branch (Leaf 6) (Leaf 7)
--      tcompose (+) t1 t2 is
 -- Branch (
 --          Branch 
 --            (Branch (Leaf 7) (Leaf 8)) 
 --            (Branch (Leaf 8) (Leaf 9))
 --        ) 
 --        (Branch (Leaf 9) (Leaf 10))

tmap :: (a -> b) -> Tree a -> Tree b
tmap _ Nil = Nil
tmap f (Leaf a) = Leaf (f a)
tmap f (Branch l r) = Branch (tmap f l) (tmap f r)

tcompose :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
tcompose f (Leaf a) t2 = tmap (f a) t2 
tcompose f (Branch l r) t2 = 
    Branch (tcompose f l t2) (tcompose f r t2)

-- the intuition is that to apply f to whole t2 you need to map
-- important is the concept of currying: (f a) is a partial function

-- 2) Define a purely functional (i.e. non destructive)
-- version of the reverse presented in Es. 1.3.
-- which takes a binary tree and keeps 
-- it structure, but “reversing” all the values in the leaves
-- E.g. t1 = Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3), 
-- E.g. revtree t1 is Branch (Branch (Leaf 3) (Leaf 2)) (Leaf 1).

tfoldl :: (a -> b -> b) -> b -> Tree a -> b
tfoldl _ Nil = Nil

revtree :: Tree a -> Tree a
revtree t = revtree' t $ foldl (\x y -> y:x) [] t where
    revtree' Nil xs = xs
    revtree' (Leaf x) xs = x:xs
    revtree' (Branch l r) xs = revtree'



-- is this distruptive?