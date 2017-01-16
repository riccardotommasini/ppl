module ESE13012017 where

import Control.Applicative
import Control.Monad

type Log = [String]
newtype Logger a = Logger { run :: (a, Log)}

logmap :: (a -> b) -> Logger a -> Logger b
logmap f log =
    let (a, l) = run log
        n = f a
    in Logger(n, l)

instance (Show a) => Show (Logger a) where
    show (Logger a) = show a

instance (Eq a) => Eq (Logger a) where
    Logger (x,y) /= Logger (a,b) = (x /= a) || (y /= b)

instance Functor Logger where
    fmap = logmap

lapp :: Logger (a -> b) -> Logger a -> Logger b 
lapp lf lg =
    let (f, llf) = run lf
        ln = logmap f lg
        (a, l) = run ln
    in Logger(a, llf ++ l)

instance Applicative Logger where
    pure a = Logger(a, [])
    (<*>) = lapp


-- Define the Logger Monad
-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   >>= is usually the only thing you have to implement

-- Monadic Laws

-- Left identity:return a >>= f is equivalent to implementing f a
-- f has the type (a -> m b) so it returns a monad
-- this means that the minimal context to return 
-- is just applying f to a

-- Right identity:  m >>= return equivalent to just m

-- When we feed monadic values to functions by using >>=, 
-- those functions take normal values and return monadic ones. 
-- return is also one such function, if you consider its type. 

-- Associativity:   (m >>= f) >>= g ≡   m >>= (\x -> f x >>= g)
-- nesting doesn't matter when in 
-- a chain of monadic function applications

-- All monads are Applicative even though 
-- it is not explicitly required, because of historical 
-- haskell impl

instance Monad Logger where
   m >>= f = -- :: Logger a -> (a -> Logger b) -> Logger b
    let (a, la) = run m
        mb = f a
        (b, lb) = run mb
    in Logger(b, lb ++ la)


-- Define a function that takes a number, add one and log the operation

logAddOne :: (Num a) => a -> Logger a
logAddOne a = Logger (a+1, ["+1"])

-- Define a function that takes a number, doubles it and log the operation

logMultiplyTwo :: (Num a) => a -> Logger a
logMultiplyTwo a = Logger (a*2, ["*2"])

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
    p1 <- logAddOne v
    m2 <- logMultiplyTwo p1
    return m2

-- Access the monadic state directly (see State Monad)
record :: String -> Logger ()
record s = Logger((), [s])

try :: (Show a) => a -> Logger ([a])
try x = do
    record("First Op")
    return [x]

-- Binary tree

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Eq)

singletonM :: (Show a) => a -> Logger (Tree a)
singletonM x = do
    record("Created a singleton " ++ show x)
    return (Node x EmptyTree EmptyTree)

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
        r <- treeInsertM x right
        return (Node a left r)



-- Write a function that sums over the element of the tree (Hint: fold for a tree)
treeSumM :: Num a => Logger (Tree a) -> Logger a
treeSumM t = fmap (treeFoldr (+) 0) t

-- Define the treeBalancedM function that logs if the tree is balanced
-- Hint: define an andM function to do "logical and"(&) on your monad

andM :: Logger Bool -> Logger Bool -> Logger Bool

-- 
treeBalancedM :: Tree a -> Logger Bool


















