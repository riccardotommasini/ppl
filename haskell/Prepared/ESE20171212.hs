module ESE20171212 where

import Control.Applicative
import Control.Monad

-- Simplified version of State Monad



type Log = [String]
newtype Logger a = Logger { run :: (a, Log)} 

-- data vs newtype?
--  data can only be replaced with newtype if the type has exactly one 
    -- constructor with exactly one field inside it.
instance (Show a) => Show (Logger a) where
    show (Logger a) = show a 

instance (Eq a) => Eq (Logger a) where
    Logger (x,y) /= Logger (a,b) = (x /= a) || (y /= b)

-- Define an instance of Functor for Logger

logmap :: (a -> b) -> Logger a -> Logger b 
logmap f log = 
    let a, l) = run log 
        n = f a
    in Logger (n, l)

instance Functor Logger where
    fmap = logmap

-- Define an instance of Applicative Functor for Logger

lapp :: Logger (a -> b) -> Logger a -> Logger b
lapp lf lg =
    let (f, llf) = run lf
        ln = logmap f lg
        (a, l) = run ln
    in Logger (a, l ++ llf)


instance Applicative Logger where
    pure a = Logger (a,[])
    (<*>) = lapp

-- Define the Logger Monad
-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   >>= is usually the only thing you have to implement
-- Monadic Laws
-- Left identity:return a >>= f ≡ f a
-- f has the type (a -> m b) so it returns a monad
-- this means that the minimal context to return 
-- is just applying f to a

-- Right identity:	m >>= return≡	m
-- When we feed monadic values to functions by using >>=, 
-- those functions take normal values and return monadic ones. 
-- return is also one such function, if you consider its type. 

-- Associativity:	(m >>= f) >>= g ≡	m >>= (\x -> f x >>= g)

-- All monads are Applicative even though 
-- it is not explicitly required, because of historical 
-- haskell impl

instance Monad Logger where
    return = pure
    m >>= f = let (a, w) = run m
                  n      = f a
                  (b, x) = run n
              in Logger (b, w ++ x)

-- differently from State, Logger takes only one parameter, i.e.,
-- we can write a non-parametric monad implementation

-- Define a function that takes a number, add one and log the operation
logPlusOne :: (Num a) => a -> Logger a
logPlusOne a = Logger (a+1, ["Add One"])

-- Define a function that takes a number, doubles it and log the operation
logMultiplyTwo :: (Num a) => a -> Logger a
logMultiplyTwo a = Logger (a*2, ["Multiply Two"])

-- Using DO notation (gluing together monadic values)
-- let l = Logger (42, ["The Answer"])
-- Error: logMultiplyTwo l 
-- Works: l >>= logMultiplyTwo
-- logOpsLambda = (\lg -> lg >>= logPlusOne >>= logMultiplyTwo )

-- Define a function that takes a logger, adds one, doubles the value
-- and logs all the operations

logOps :: (Num a) => Logger a -> Logger a
logOps lg = do
    v <- lg
    p1 <- logPlusOne v -- the monads hides some computation, 
    m2 <- logMultiplyTwo p1 -- i.e., the extraction of (a,list) and the application of f on a
    return m2 -- necessary to close the monadic chain 


-- Define a record function to record things in the log
record :: String -> Logger ()
record s = Logger ((), [s])

-- Binary tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

treeFoldr :: (b -> a -> a) -> a -> Tree b -> a
treeFoldr f acc EmptyTree = acc	
treeFoldr f acc (Node b left right) = treeFoldr f (f b (treeFoldr f acc right)) left

-- We want to add logging to our tree, so define singletonM, treeInsertM and treeSumM
-- that logs the operations performed on the tree during execution
singletonM :: (Show a) => a -> Logger (Tree a)
singletonM x = do
    record ("Created singleton " ++ show x) -- monad hides record >>
    return (Node x EmptyTree EmptyTree) -- return the Logger of tree

-- let nums = [8,6,4,1,7,3,5]
-- :t foldM
-- let numsTree = foldM treeInsertM EmptyTree nums
-- (foldM because treeInsertM returns a monad instead of the standard data structure)
treeInsertM :: (Ord a, Show a) => Tree a -> a -> Logger (Tree a)
treeInsertM EmptyTree x = singletonM x
treeInsertM (Node a left right) x
    | x == a = do
        record ("Inserted " ++ show x)
        return (Node x left right)
    | x < a = do
        l <- treeInsertM left x
        return (Node a l right)
    | x > a = do
        r <- treeInsertM right x
        return (Node a left r)

-- HOMEWORKS

-- Write a function that sums over the element of the tree (Hint: fold for a tree)
treeSumM :: (Show a, Num a) => Logger (Tree a) -> Logger a
treeSumM t = do
    v <- fmap (treeFoldr (+) 0) t
    record ("Summed " ++ show v)
    return v

-- Define a function that logs if the tree is balanced
-- Hint: define an andM function to do "logical and"(&)
-- on your monad
andM :: Logger Bool -> Logger Bool -> Logger Bool
andM l1 l2 = do
    c1 <- l1
    c2 <- l2
    return (c1 && c2)

treeBalancedM :: Tree a -> Logger Bool
treeBalancedM EmptyTree = do
    record "An empty tree is always balanced"
    return True
treeBalancedM (Node _ EmptyTree EmptyTree) = do
    record "A single node tree is always balanced"
    return True
treeBalancedM (Node _ EmptyTree _) = do
    record "Unbalanced!"
    return False
treeBalancedM (Node _ _ EmptyTree) = do
    record "Unbalanced!"
    return False
treeBalancedM (Node _ left right) = andM (treeBalancedM left) (treeBalancedM right)






























