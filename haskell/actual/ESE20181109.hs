module ESE20181109 where

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = fib (n -1 ) + fib (n - 2)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

newtype Str = Str [Char] deriving (Show)

myRange :: Int -> Int -> [Int]
myRange a b = if a > b
    then error  "Low > High"
    else if a < b
        then a : myRange (a + 1) b
        else [a]

myRange2 :: Int -> Int -> [Int]
myRange2 a b
    | a > b = error  "Low > High"
    | a == b = [a]
    | a < b = a : myRange2 (a + 1) b


makeinf :: Int -> [Int]
makeinf n = [n,n+1..]

-- My FOLD

myfoldl ::  (a -> a -> a) -> a -> [a] -> a
myfoldl f a [] = a
myfoldl f a (x:xs) = myfoldl f (f a x) xs



myzip :: [a] -> [b] -> [(a,b)]
myzip _ r@[] = []
myzip l@[] _ = []
myzip (x:xs) (y:ys) = (x,y):myzip xs ys


data TrafficLights = Red | Green | Yellow


instance Eq TrafficLights where     
    Red == Red = True 
    Green == Green = True 
    Yellow == Yellow = True 
    _ == _ = False


data Point a b = Point a b 

instance (Show a) => (Show b) => Show (Point a b) where
    show (Point x y) = show x ++ " " ++ show y

getx (Point x _) = x
gety (Point _ y) = y

data Tree a = Leaf a | Branch (Tree a) (Tree a)

data Pair a b = Pair a b


