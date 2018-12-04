module ESW20181123 where

    data Tree a = Nil | Leaf a | Branch (Tree a) (Tree a) deriving (Show,Eq)

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

    oddt :: Integral a => Tree a -> Bool
    oddt Nil = True
    oddt (Leaf a) = odd a
    oddt (Branch l r) = oddt l && oddt r 

    -- oddMaxTree :: (Int a) => Tree a -> [Tree a]
    -- oddMaxTree Nil = []
    -- oddMaxTree x@(Leaf a) 
    --             | oddt x = [x]
    --             | otherwise = []
    -- oddMaxTree b@(Branch l r) 
    --            | oddt b = [b]
    --            | oddt l && not oddt r = [l] ++ oddMaxTree r
    --            | oddt r && not oddt l = [r] ++ oddMaxTree l
    --            | otherwise = oddMaxTree l + oddMaxTree r


    oddMaxTree :: Integral a => Tree a -> [Tree a]
    oddMaxTree t = oddMaxTree' t [] oddt where
        oddMaxTree' Nil ts _ = ts
        oddMaxTree' l@(Leaf a) ts p
                    | p l  = l:ts
                    | otherwise = ts
        oddMaxTree' b@(Branch l r) ts p
                    | p b = b:ts
                    | otherwise = ts ++ oddMaxTree l ++ oddMaxTree r


    -- revtree :: Tree a -> Tree a
    -- t1 = Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3)
    -- revtree t1 ->  Branch (Branch (Leaf 3) (Leaf 2)) (Leaf 1)

    fringe :: Tree a -> [a]
    fringe t = fringe' t [] where
        fringe' Nil xs = xs
        fringe' (Leaf a) xs = a:xs
        fringe' (Branch l r ) xs = xs ++ fringe l ++ fringe r


    helper :: Tree a -> [a] -> (Tree a, [a])
    helper Nil xs = (Nil, xs)
    helper (Leaf a) (x:xs) = (Leaf x, xs)
    helper (Branch l r) xs = let (l', xs') = (helper l xs) 
                                 (r', xs'') = (helper r xs')
                        in (Branch l' r', xs'')


    revtree :: Tree a -> Tree a
    revtree t = t1 
        where (t1,_) = helper t (reverse (fringe t))







    t2concat :: Tree a -> Tree a -> Tree a
    t2concat t Nil = t
    t2concat Nil t = t
    t2concat t1 t2 = Branch t1 t2

    tconcat t = tfoldr t2concat Nil t

    tconcatmap f t = tconcat $ tmap f t


    instance Applicative Tree where
        pure = Leaf
        fs <*> ts = tconcatmap (\f -> tmap f ts) fs












