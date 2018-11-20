module Bintree where


-- Binary tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance Show a => Show (Tree a) where
    show (Leaf a) = show a
    show (Branch x y) = "<" ++ show x ++ " | " ++ show y ++ ">"

-- 

instance Eq Data where
	func = 
treeFoldr :: (b -> a -> a) -> a -> Tree b -> a
treeFoldr f acc EmptyTree = acc 
treeFoldr f acc (Node b left right) = treeFoldr f (f b (treeFoldr f acc right)) left

instance (Eq t) => Eq (Tree t) where
    Empty == Empty = True
    Leaf a == Leaf b = a == b
    Node x y == Node a b = (x == a) && (y == b)
    _ == _ = False

-- foldable

btfoldr f z Empty = z
btfoldr f z (Leaf x) = f x z
btfoldr f z (Node x y) = btfoldr f (btfoldr f z y) x

-- i can use foldr normally now

-- data Maybe a = Nothing | Just a

-- instance Foldable Maybe where
--     foldr _ z Nothing = z
--     foldr f z (Just x) = f x z

-- instance Functor Maybe where
--     fmap _ Nothing = Nothing
--     fmap f (Just x) = f x

tmap _ Empty = Empty
tmap f (Leaf x) = Leaf $ f x
tmap f (Node x y) = Node (tmap f x) (tmap f y)
