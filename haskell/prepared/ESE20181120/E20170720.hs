module E20170720 where

-- 1) Define a ternary tree data structure, called Ttree, 
---   in which every node contain both a value and a color, which can be either
--    yellow or blue. You can assume that a tree cannot be empty.


data Color = Blue | Yellow deriving (Eq, Show)
data Ttree a = Tleaf a Color | TBranch a Color (Ttree a) (Ttree a) (Ttree a) deriving Show

-- 2) Make Ttree an instance of Functor, i.e. implement a map


-- 3) Make Ttree an instance of Foldable, i.e. implement foldr


-- 4) Define a function yellowSubTrees, 
-- which returns a list containing all the maximal subtrees 
-- of a given Ttree that are all made of
-- yellow nodes.


--

noBlues :: Ttree a -> Bool
noBlues (Tleaf _ Blue) = False 
noBlues (Tleaf _ Yellow) = True
noBlues (TBranch v Blue l m r) = False
noBlues (TBranch v Yellow l m r) = (noBlues l) && (noBlues m) && (noBlues r)

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

