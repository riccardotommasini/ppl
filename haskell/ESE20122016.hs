module ESE201216 where

import Control.Applicative
import Control.Monad

-- More on SUM types 

data Point = Point Float Float deriving (Show)

getx (Point x _) = x
gety (Point _ y) = y

data ClikePoint = ClikePoint {x, y :: Float}

pointx (ClikePoint x _) = x
pointy (ClikePoint _ y) = y

-- Useful for complex stuff
data Person1 = Person1 String String Int Float String String deriving (Show)  

data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show)

-- NB Show is different

-- already defined as x and y named as the field

-- type is used for type-synonyms String = [Char]
-- newtype is used for type-synonyms that do are not consider equals String != [Char]
-- data constructor has to be defined and it is not lazy as for "data"
--- data and data can always replace newtype
--- newtype can only replace replace data
--- if the type has exactly one constructor with exactly one field inside it.
newtype Str = Str [Char] deriving (Show)
type S = [Char]

-- Function Composition (.)

dd = (*2) . (1+)

-- Strict Version of Foldl

foldl' f z [] = z
foldl' f z (x:xs) = let z' = f z x 
                        in seq z' (foldl' f z' xs)

-- seq
-- $     f $ x = x (f x)
-- $!    f $! x = seq x (f x)


-- Towards Monads

data Tree a = Empty | Leaf a | Node (Tree a) (Tree a)

-- instance Eq Tree where
--          ... wring because Tree is not a a concrete type
--

-- instance Eq Tree a where
--     Empty == Empty = True
--     Leaf a == Leaf b = a == b
--     Node x y == Node a b = (x == a) && (y == b)
--     _ == _ = False

-- almost there
-- Tree is not a concrete type
-- (Tree a) is a concrete type, but we are saying that 
-- all the type in the form Tree a, where a is any, are an instance of Eq.
-- Do we have any assurance that a is an instance of Eq?

instance (Eq t) => Eq (Tree t) where
    Empty == Empty = True
    Leaf a == Leaf b = a == b
    Node x y == Node a b = (x == a) && (y == b)
    _ == _ = False

-- foldable

btfoldr f z Empty = z
btfoldr f z (Leaf x) = f x z
btfoldr f z (Node x y) = btfoldr f (btfoldr f z y) x

instance Foldable Tree where
    foldr = btfoldr

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

instance Functor Tree where
    fmap = tmap 

-- f is not a concrete type but a 
--type constructor that takes one type parameter
-- class Functor f where  
--  fmap :: (a -> b) -> f a -> f b  

-- instance Functor [] where  
--   fmap = map

-- That's it! Notice how we didn't write 
-- instance Functor [a] where, because from 
-- fmap :: (a -> b) -> f a -> f b, 
-- we see that the f has to be a type constructor that takes one type. 
-- [a] is already a concrete type (of a list with any type inside it),
-- while [] is a type constructor that takes one type and can produce 
-- types such as [Int], [String] or even [[String]].

type Log = [String]
newtype Logger a = Logger { runlogger :: (a, Log)}

instance Show a => Show (Logger a) where
    show (Logger a) = show a 

instance (Eq l) => Eq (Logger l) where
    Logger (a,l) /= Logger (b,k) = (a /= b) || (l /= k)

-- Define a function that takes a number, add one and log the operation
logPlusOne :: (Num a) => a -> Logger a
logPlusOne a = Logger (a+1, ["Add One"])

-- Define a function that takes a number, doubles it and log the operation
logMultiplyTwo :: (Num a) => a -> Logger a
logMultiplyTwo a = Logger (a*2, ["Multiply Two"])

-- Define a function that takes a logger, adds one, doubles the value
-- and logs all the operations
logOps :: (Num a) => Logger a -> Logger a
logOps lg = do
    v <- lg
    p1 <- logPlusOne v
    m2 <- logMultiplyTwo p1
    return m2

-- Define a record function to record things in the log
record :: String -> Logger ()
record s = Logger ((), [s])

-- Define an instance of Functor for Logger
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

lmap :: (a -> b) -> (Logger a) -> (Logger b)
lmap f lg =  
    let (a, l) = runlogger lg -- using accessor method
        n = f a
    in Logger (n, l) -- l is an invariant

instance Functor Logger where
    fmap = lmap

-- fmap id = id where id is the identity function
-- fmap (f . g) = fmap f . fmap g (homomorphism)

-- Applicative functors
-- (*) is binary operation
-- let l = Logger (1, ["ciao"])
-- :t fmap (*) l
-- fmap (*) l :: Num a => Logger (a -> a)
-- is a function wrapped in Logger

-- let a = fmap (*) [1,2,3,4]  
-- :t a  
-- a :: [Integer -> Integer]  
-- fmap (\f -> f 9) a  
-- [9,18,27,36]  

-- Define an instance of Applicative Functor for Logger
-- class Functor f => Applicative f where
--   pure :: a -> f a takes a value and return f containing it
--   (<*>) :: f (a -> b) -> f a -> f b -- like fmap but it takes an f containing  a function

lapp :: (Logger (a -> b)) -> Logger a -> Logger b
lapp lf lg =
    let (f, llf) = runlogger lf --extract function
        nl = lmap f lg --- applies is through map
        (a, l) = runlogger nl -- extract result logger
    in Logger (a, l++llf) -- create result (append lists)

instance Applicative Logger where
    pure a = Logger (a,[])
    (<*>) = lapp



