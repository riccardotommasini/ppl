
module TUT20190115A where

    data Tree a = Tree a [Tree a] deriving Show

    data Tree2 a = Leaf a | Branch a [Tree2 a] deriving Show

    visit :: Tree a -> [a]
    visit (Tree a []) = [a]
    visit (Tree a childern)  = foldl (++) [a] (map visit childern)

    -- visit2 (Leaf a ) = [a]
    -- visit2 (Tree a leaves) = foldl (++) [a] (map visit2 leaves) 

    -- concatMap :: (a -> a) -> [[a]] -> [a]
    -- concatMap  f l = map f $ foldl (++) [] l

    countnodes :: Tree a -> Int
    countnodes (Tree a []) = 1
    countnodes (Tree a l ) = foldl (+) 1 (map countnodes l)
 
    -- instance Eq (Tree a) where
    --     t1 == t2 = (countnodes t1) == (countnodes t2) 

    instance (Eq a) => Eq (Tree a) where
        t1 == t2 = (visit t1) == (visit t2) 

    zip2list :: [(a,a)] -> [a]
    zip2list [] = []
    zip2list ((x,y):xs) = [x,y] ++ zip2list xs

    






