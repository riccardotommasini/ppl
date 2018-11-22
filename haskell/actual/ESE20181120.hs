module Bintree where
    
    data Fruit = Apple | Orange | Pears deriving (Show, Eq)

    data Treasure = Gold | Silver deriving (Show, Eq)

    mida :: a -> Treasure
    mida _ = Gold

    data Tree a = Nil | Leaf a | Branch (Tree a) (Tree a) deriving Show



    instance Eq a => Eq (Tree a) where
        Nil == Nil = True 
        (Leaf x) == (Leaf y) = x == y
        (Branch k t) == (Branch x y) = k == x && t == y
        _ == _ = False


    tmap :: (a -> b) -> Tree a -> Tree b
    tmap _ Nil = Nil
    tmap f (Leaf a) = Leaf (f a)
    tmap f (Branch l r) = Branch (tmap f l) (tmap f r)


    instance Functor Tree where
        fmap = tmap


    tcompose :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
    tcompose f Nil t = Nil
    tcompose f (Leaf a) t = tmap (f a) t
    tcompose f (Branch l r) t = Branch (tcompose f l t) (tcompose f r t) 

    tfoldr :: (a -> b -> b) -> b -> Tree a -> b
    tfoldr f b Nil = b
    tfoldr f b (Leaf a) = f a b
    tfoldr f b (Branch l r) = tfoldr f (tfoldr f b r) l

    tcount :: Tree a -> Int
    tcount Nil = 0
    tcount (Leaf _ ) = 1
    tcount (Branch l r) = (tcount l) + (tcount r)

    myodd:: Int -> Bool
    myodd a = mod a 2 == 0


-- 1) Define a tcompose operation, 
--    which takes a function f and two trees, t1 and t2, 
--    and returns a tree with the same structure as t1,
--    but with leaves replaced by subtrees 
--    having the same structure of t2: 
--    each leaf is obtained by applying f 
--    to the value stored in the
--    previous leaf, and the corresponding value in t2.

-- E.g. t1 = Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3)], 
--      t2 = Branch (Leaf 6) (Leaf 7)
--      tcompose (+) t1 t2 is
 -- Branch (
 --          Branch 
 --            (Branch (Leaf 7) (Leaf 8)) 
 --            (Branch (Leaf 8) (Leaf 9))
 --        ) 
 --        (Branch (Leaf 9) (Leaf 10))


-- Homework 
-- Function maxsubtree that takes a condition and a tree and returns the list of maximum subtrees that 
-- satisfy the condition

    maxsubtree:: (a -> Bool) -> [Tree a] -> Tree a -> [Tree a]
    maxsubtree cond b Nil = b
    maxsubtree cond b (Leaf a) = if (testcond cond (Leaf a))
                                 then (Leaf a):b
                                 else b
    maxsubtree cond b (Branch l r) = if testcond cond (Branch l r) 
                                     then (Branch l r):b
                                     else maxsubtree cond (maxsubtree cond b r) l  

    testcond:: (a->Bool)->Tree a-> Bool
    testcond cond Nil = True
    testcond cond (Leaf a) = cond a
    testcond cond (Branch l r) = (testcond cond l) && (testcond cond r)

