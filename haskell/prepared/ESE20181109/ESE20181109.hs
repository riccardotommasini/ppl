module ESE20181109 where

-- Reverse a list

-- myReverse [1,2,3]
-- myReverse [1..20]
-- myReverse [0.1,0.2,0.3]

-- Reverse 
-- myReverse2 :: [a] -> [a]
-- myReverse2 [1,2,3]
-- myReverse2 [0.1,0.2,0.3]
-- myReverse2 [0.1, 0.3 .. 1] Watch out when using floating point numbers in ranges!
-- myReverse2 ['a', 'b', 'c']
-- NULL

myReverse :: [Int] -> [Int]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]


empty :: [a] -> Bool
empty [] = True
empty (_:xs) = False 

-- if vs guards

myRange :: Int -> Int -> [Int] -- currying
myRange a b = if a > b
    then error "Low > High"
    else if a < b
        then a : myRange (a + 1) b
        else [a]

-- guards matching
myRange2 :: Int -> Int -> [Int]
myRange2 a b 
    | a > b = error "Low > High"
    | a == b = [a]
    | a < b = myRange2 (a + 1) b

-- guards vs pattern matching

--fibonacci
fib :: Int -> Int
fib n 
    | n == 0 = 0
    | n == 1 = 1  
    | n == 2 = 1
    | n > 2 = fib (n - 1) + fib (n - 2)

fib2 :: Int -> Int
fib2 0 = 0
fib2 1 = 1
fib2 2 = 1
fib2 n = fib2 (n - 1) + fib2 (n - 2)


-- infinite list, use "take"! 
-- This makes sens in presence of call-by-need
makeinf :: Int -> [Int]
makeinf a = a : myRange3 (a + 1)

-- :t take
-- take 10 $ makeinf 10

-- More on infinite Lists
-- cycle (task a list and cycles it into an infinite list)

myCycle :: [a] -> [a]
myCycle [] = error "Empty List"
myCycle (x:xs) = x:xs ++ myCycle(x:xs)

-- Higher-Oder Functions
-- Map

myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = (f x): myMap f xs 

-- myMap (\x -> 1 + x) [1,2,3]

-- FoldL

myFoldL :: (a -> a -> a) -> a -> [a] -> a
myFoldL f x [] = x
myFoldL f x (y:ys) = myFoldL f (f x y) ys

-- myFoldL (\x y -> y + x) 0 [1,2,3]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs)
    | f x = x: myFilter f xs
    | otherwise = myFilter f xs

-- More on List
-- Zip

myZip :: [a] -> [b] -> [(a,b)]
myZip l [] = []
myZip [] l = []
myZip (x:xs) (y:ys) = (x,y):(myZip xs ys)
-- myZip [1,2,3] ['a','b','c']

-- L:List comprehensions
rightTriangles :: [(Integer, Integer, Integer)]
rightTriangles = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a + b + c == 24]

-- WARNING: infinite list, use "take"!
rightTriangles2 :: [(Integer, Integer, Integer)]
rightTriangles2 = [(a,b,c) | c <- [1,2..], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2] 

-- And there's our answer! This is a common pattern in functional programming. 
-- You take a starting set of solutions and then you apply 
-- transformations to those solutions and filter them until you get the right ones.

-- List Comprehension
-- [x*2 | x <- [1..10]]

-- Haskell has type inference. If we write a number, we don't have to tell Haskell it's a number. 
-- It can infer that on its own, so we don't have to explicitly write out the types of our functions and expressions to get things done. 

factorial :: Int -> Int
factorial n = product [1..n]

factorialBig :: Integer -> Integer
factorialBig n = product [1..n]


-- Types
-- :t 42, "ciao", 1 == 1, head (car), tail (cdr)
-- :t (==), elem

-- Eq and Ord types that support equality tests and ordering
-- Show presented as string

-- Pack
-- => requires a to be an Eq

packHelper :: Eq a => [a] -> [[a]] -> [a] -> [[a]]
packHelper [] acc sub = sub:acc
packHelper (x:xs) acc [] = packHelper xs acc [x]
packHelper (x:xs) acc (y:ys)
    | x == y = packHelper xs acc (x:sub)
    | otherwise = packHelper xs (sub:acc) [x]
    where sub = y:ys

--encapsulate each element of a list into a list
pack :: Eq a => [a] -> [[a]]
pack input = reverse (packHelper input [] [])

-- Encode
encode :: Eq a => [a] -> [(a, Int)]
encode input = zip (map head packed) (map length packed)
    where packed = pack input

-- names after the where are visible across the guards

-- BMI - Indice massa corporea
bmi :: (RealFloat a) => a -> a -> String  
bmi weight height
    | weight / height ^ 2  < skinny = "Underweight!"  
    | weight / height ^ 2  <= normal = "Just normal!"  
    | weight / height ^ 2  <= fat    = "Fatty!"  
    | otherwise     = "OMG!"
    where skinny = 18.5  
          normal = 25.0  
          fat = 30.0

-- make it step by step

cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea 

 -- let <bindings> in <expression> 
-- The names that you define in the 
-- let part are accessible to the expression after the in part. 


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let right = quicksort [a | a <- xs, a <= x]
        left = quicksort [a | a <- xs, a > x]
    in right ++ [x] ++ left


-- Binary trees
-- let empty = EmptyTree
-- let singleNode = Node 10 EmptyTree EmptyTree
-- let twoNodes = Node 5 empty singleNode
-- ...
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- Implementing a type class
-- 
-- Definition from the Prelude:
-- class Eq a where
--     (==) :: a -> a -> Bool
--     (/=) :: a -> a -> Bool
--     x == y = not (x /= y)
--     x /= y = not (x == y)
--
-- This means that a) we have to implement both "equals" and "not equals"
-- and b) since "x is equal to y if x is not equal to y" and viceversa,
-- we can just define "equals" or "not equals" and Haskell will infer the
-- other one.

-- SUM type
data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"


-- Product type
data Point = Point Float Float deriving (Show)

getx (Point x _) = x
gety (Point _ y) = y

-- alternative syntax
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
-- : t (.)
-- (.) :: ( b -> c ) -> ( a -> b ) -> a -> c
dd = (*2) . (1+)

-- Strict Version of Foldl
-- Not lazy
foldl' f z [] = z
foldl' f z (x:xs) = let z' = f z x 
                        in seq z' (foldl' f z' xs)

-- seq
-- $     f $ x = x (f x)
-- $!    f $! x = seq x (f x)


