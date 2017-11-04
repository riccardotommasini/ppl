module A160311 where

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]


myRange :: Int -> Int -> [Int]
myRange a b = if a > b
	then error "Low > High"
	else if a < b
		then  (myRange a (b-1)) ++ [b]
		else [a]

myRange2 :: Int -> Int -> [Int]
myRange2 a b
	| a > b  = error "error"
	| a == b = [a]
	| otherwise = a : myRange2 (a + 1) b


myFoldL :: (a -> a -> a) -> a -> [a] -> a
myFoldL f x [] = x
myFoldL f x (y:ys) = myFoldL f (f x y) ys

myZip :: [a] -> [b] -> [(a,b)]
myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = (x,y):(myZip xs ys)


factorial :: Integer -> Integer
factorial n = product [1..n]


data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
	Red == Red = True
	Yellow == Yellow = True
	Green == Green = True 
	_ == _ = False

instance Ord TrafficLight where
	compare Red _  = LT 
	compare Green _ = GT
	compare Yellow Red = GT
	compare Yellow Green = LT

-- PRODUCT


data Point = Point Float Float

getX (Point x _ ) = x
getY (Point _ y ) = y


data Person = Person { firstName :: String,
					   age :: Int,
					   email :: String
					 }

instance Show Person where
	show (Person x y z)  = 
		"Name " ++ x ++ " Age " ++  (show y ) ++ " Email " ++ z




































