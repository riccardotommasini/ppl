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



-- 2) Define a purely functional (i.e. non destructive)
-- version of the reverse presented in Es. 1.3.
-- which takes a binary tree and keeps 
-- it structure, but “reversing” all the values in the leaves
-- E.g. 

-- t1 = Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3), 
-- E.g. revtree t1 is Branch (Branch (Leaf 3) (Leaf 2)) (Leaf 1).

-- reduce  tree to a list of value
fringe :: Tree a -> [a]
fringe t = fringe' t [] where
    fringe' Nil xs = xs
    fringe' (Leaf x) xs = x:xs
    fringe' (Branch l r) xs = (fringe l) ++ (fringe r) ++ xs

--reverse the tree
revtree' Nil xs = (Nil, xs)
revtree' (Leaf v) (x:xs) = (Leaf x, xs)
revtree' (Branch l r) xs = let (l', xs') = revtree' l xs 
                               (r', xs'') = revtree' r xs'
                           in (Branch l' r', xs'')

revtree :: Tree a -> Tree a
revtree t = t1
    where (t1, _) = revtree' t (reverse (fringe t))

-- 4) Define a function yellowSubTrees, 
-- which returns a list containing all the maximal subtrees 
-- of a given Ttree that are all made of
-- yellow nodes.


--- 
colored trees


noBlues :: Tree Color -> Bool
noBlues (Leaf Blue) = False 
noBlues (Leaf Yellow) = True
noBlues (Branch Blue l m r) = False
noBlues (Branch Yellow l m r) = (noBlues l) && (noBlues m) && (noBlues r)

yellowSubTrees :: Ttree a -> [Ttree a]
yellowSubTrees t = yellowSubTrees' t [] where
    yellowSubTrees' (Tleaf _ Blue) xs = xs
    yellowSubTrees' x@(Tleaf _ Yellow) xs = x:xs
    yellowSubTrees' x@(TBranch _ color l m r) xs 
                    | (noBlues x) = x:xs
                    |   = (yellowSubTrees l) ++ (yellowSubTrees m) ++ (yellowSubTrees r) 

-- all the branches that are yellow
-- and whose siblings are yellow too

-- E.g. in the case of the following tree, where yellow nodes are depicted in white, the returned list contains the three outlined
-- subtrees.


--- Applicative


--- see concact [[1],[2],[3]]
--- lists are functors
--- lists are applicative
--- :t (<*>)
--- origin of the symbol
--- 

-- instance Applicative [] where
-- pure x = [x]
-- fs <*> xs = concatMap (\f -> map f xs) fs
-- comment on fs and xs


-- let's make our tree instance of applicative

-- tconcatmap :: Foldable t => (a -> Tree b) -> Tree a -> Tree b
