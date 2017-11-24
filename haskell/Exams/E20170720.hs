module E20170720 where

-- 1) Define a ternary tree data structure, called Ttree, 
---   in which every node contain both a value and a color, which can be either
--    yellow or blue. You can assume that a tree cannot be empty.

data Color = Blue | Yellow deriving (Eq, Show)
data Ttree a = TLeaf a Color | TBranch a Color (Ttree a) (Ttree a) (Ttree a) deriving Show

-- 2) Make Ttree an instance of Functor, i.e. implement a map

ttreeMap :: (a -> b) -> Ttree a -> Ttree b
ttreeMap f (TLeaf a c) = (TLeaf (f a) c)
ttreeMap f (TBranch a c x y z) = (TBranch (f a) c (ttreeMap f x) (ttreeMap f y) (ttreeMap f z))

-- 3) Make Ttree an instance of Foldable, i.e. implement foldr

-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

ttreeFoldR :: (a -> b -> b) -> b -> Ttree a -> b
ttreeFoldR f b (TLeaf a _) = (f a b)
ttreeFoldR f b (TBranch a _ x y z) = ttreeFoldR f (ttreeFoldR f (ttreeFoldR f b x) y) z

-- better syntax
--ttreeFoldR' :: (a -> b -> b) -> b -> Ttree a -> b
-- show that the derived type is correct
ttreeFoldR' f b (TLeaf a _) = (f a b)
ttreeFoldR' f b (TBranch a _ x y z) =
    let v1 = ttreeFoldR' f b x
        v2 = ttreeFoldR' f v1 y
        v3 = ttreeFoldR' f v2 z
    in f a v3

-- 4) Define a function yellowSubTrees, 
-- which returns a list containing all the maximal subtrees 
-- of a given Ttree that are all made of
-- yellow nodes.

-- all the branches that are yellow
-- and whose siblings are yellow too

noBlues :: Ttree a -> Bool
noBlues (TLeaf a Blue) = False
noBlues (TLeaf a Yellow) = True 
noBlues (TBranch _ Blue _ _ _) = False
noBlues (TBranch _ Yellow x y z) = (noBlues x) && (noBlues y) && (noBlues z)

yellowSubTrees :: Ttree a -> [Ttree a]
yellowSubTrees t = yellowSubTrees' t [] where 
    yellowSubTrees' x@(TLeaf _ Blue) xs = xs
    yellowSubTrees' x@(TLeaf _ Yellow) xs = (x:xs)
    yellowSubTrees' x@(TBranch _ _ y z k) xs
        | (noBlues x) = x:xs
        | otherwise = (yellowSubTrees y) ++ (yellowSubTrees z) ++ (yellowSubTrees k)

-- why is the proposed solution better?
-- question efficiency

-- let l0 = (TLeaf 0 Yellow)
-- let l1 = (TLeaf 1 Yellow)
-- let l2 = (TLeaf 2 Yellow)
-- let l3 = (TLeaf 3 Yellow)
-- let l4 = (TLeaf 4 Blue)
-- let l5 = (TLeaf 4 Yellow)
-- let l6 = (TLeaf 4 Blue)

-- let b1 = (TBranch 1 Yellow l0 l1 l2)
-- let b2 = (TBranch 2 Yellow l3 l4 l5)
-- let b3 = (TBranch 0 Blue b1 l6 b2)

-- yellowSubTrees b3

-- E.g. in the case of the following tree, where yellow nodes are depicted in white, the returned list contains the three outlined
-- subtrees.