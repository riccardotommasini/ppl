module Bintree where

    data Tree a = Nil | Leaf a | Branch (Tree a) (Tree a) deriving Show

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

    instance Foldable Tree where
        foldr =  tfoldr


-- Homework 
-- Function maxsubtree that takes a condition and a tree and returns the list of maximum subtrees that 
-- satisfy the condition



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

-- t1 = Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3), 
-- E.g. revtree t1 is Branch (Branch (Leaf 3) (Leaf 2)) (Leaf 1).
    revtree :: Tree a -> Tree a
    revtree t = t1
        where (t1, _) = revtree' t (reverse (fringe t))


    --- 
    --- 

    notEven :: Integral a => Tree a -> Bool
    notEven (Leaf a) = mod a 2 > 0 
    notEven (Branch l r) = notEven l && notEven r

--    oddMaxSubTrees :: Integral a => Tree a -> [Tree a]
-- like this it will give one error because it is missing integral
    oddMaxSubTrees ::  Integral a => Tree a -> [Tree a]
    oddMaxSubTrees t = oddMaxSubTrees' t [] where
        oddMaxSubTrees' Nil xs = xs 
        oddMaxSubTrees' x@(Leaf a) xs 
                        | notEven x = x:xs
                        | otherwise = xs
        oddMaxSubTrees' x@(Branch l r) xs 
                        | (notEven x) = x:xs
                        | otherwise = (oddMaxSubTrees l) ++ (oddMaxSubTrees r) 

    condMaxSubTrees :: Tree a -> (Tree a -> Bool) -> [Tree a]
    condMaxSubTrees t p = condMaxSubTrees' t p [] where
            condMaxSubTrees' l@(Leaf a) p ts 
                            | p l = l:ts
                            | otherwise = ts
            condMaxSubTrees' b@(Branch l r) p ts 
                            | p b = b:ts
                            | otherwise = (condMaxSubTrees l p) ++ (condMaxSubTrees r p) 

    oddMaxSubTrees2 t = condMaxSubTrees t notEven
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

    tconc :: Tree a -> Tree a -> Tree a
    tconc t Nil = t
    tconc Nil t = t
    tconc t1 t2 =  Branch t1 t2
    -- here I actually made an implicit assumption. I decided what tree concatenation means

    tconcat t = tfoldr tconc Nil t

    tconcatmap f t = tconcat $ tmap f t

    instance Applicative Tree where
        pure = Leaf
        fs <*> xs = tconcatmap (\f -> tmap f xs) fs
    --

