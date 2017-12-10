module ESE20171205 where

import Control.Monad

-- State Monad (Control.Monad.State)
    -- haskell pure functional nature saves functions to have side effects (i.e., modifying a global state).
    -- Function call just acts locally, computing and returning their results.
    -- But what if we need to actually modifying the global state (stateful computations)
    -- this need motivates the presence of the State Monad, that hides any edits to the global state, maintaining 
    -- the explicit computation purely functional.

newtype State st a = State {runState :: st -> (a, st)}

-- stateful computation that manipulates a state of type s resulting in a type a

    -- the Monad typeclass wants a constructor with a single value like Maybe a. 
    -- State s a has two values, so we fix s as a part of the constructor
   
instance Functor (State s) where
    fmap f (State g) = State (\st -> let (x, s) = g st in (f x, s))


-- Notice also that the action of fmap modifies the values, but doesn’t change the key mappings.

-- instance Functor (State s) where
--         fmap f ma = State $ (\(a,s) -> (f a,s)) . runState ma

-- f . g a = f (g a) -->  (\(a,s) -> (f a,s)) (runState ma) => sr -> (f a, s) Ok
-- instance Functor (State s) where
--         fmap f ma = State $ (\(a,s) -> (f a,s)) . runState ma

-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
instance Applicative (State s) where
        pure a =  State $ (\s -> (a, s))
        (State f) <*> (State d) = State $ \s -> 
                        let (f', s')   = f s
                            (f'', s'') = d s' 
                        in (f' f'', s'')

 -- The (<*>) action takes the input state, 
 -- and obtains the function f and an updated state st, 
 -- then passes the updated state s' to f, obtaining 
 -- the argument f'' and an updated state s'', and finally 
 -- we package up the application f' f'' and the final state s'' into a pair. 
 -- It is important to understand that (<*>) specifies an order 
 -- in which the state is used: left-hand (function) argument first, 
 -- then the right-hand argument.

-- Monadic Laws
-- Left identity:return a >>= f ≡ f a
-- f has the type (a -> m b) so it returns a monad
-- this means that the minimal context to return 
-- is just applying f to a

-- Right identity:  m >>= return≡   m
-- When we feed monadic values to functions by using >>=, 
-- those functions take normal values and return monadic ones. 
-- return is also one such function, if you consider its type. 

-- Associativity:   (m >>= f) >>= g ≡   m >>= (\x -> f x >>= g)

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
instance Monad (State s) where
    return = pure
    (State a) >>= f = State $ \s ->
        let (a', s') = a s
            (State b) = f a'
            (b', s'') = b s'
        in (b', s'')

-- Monadic Functions to manipulate the state

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put t = State $ \s -> ((), t)

edit :: (s -> s) -> State s ()
edit f = State $ \s -> ((), f s)

-- functions to access the internal state
evalState :: State s a -> s -> a
evalState ma s = fst (runState ma s)

execState :: State s a -> s -> s
execState ma s = snd (runState ma s)

-- TE 2013/07/05

-- Define the monadic function 
-- mapListM :: (t -> State st a) -> [t] -> State st [a]
-- that applies its first argument (another monadic function, as 
-- you can see by the signature) to every element of the list
-- passed as its second argument (i.e. like a map).

mapListM :: (t -> State st a) -> [t] -> State st [a]
mapListM f []     = do return []
mapListM f (x:xs) = do x1 <- f x -- x1 is a State, >>= is implicit
                       xs1 <- mapListM f xs -- recursive call
                       return (x1:xs1) -- wrap up a list

-- f x >>= (\x1 -> mapListM f xs >>= \xs1 -> return (x1:xs1) )


-- Define the monadic function 
-- numberList :: Num st => [st] -> State st [(st, st)],
-- **based on mapListM**, that takes as input a 
-- list of numbers and returns a list of pairs of 
-- numbers (x, y), where the first component is 
-- the same as the value x at the same position 
-- in the input list, while y is the state when x 
-- was reached. The state is incremented by x, 
-- when x is reached.

numberList :: Num st => [st] -> State st [(st, st)]
numberList l = mapListM helper l where
    helper x = do st <- get
                  put (st+x)
                  return (x, st+x)


-- HTML

type HTML = State String

string :: String -> HTML ()
string t = edit (++t)

render :: HTML a -> String
render mHTML = execState mHTML ""

-- define HTML core

tag :: String -> HTML a -> HTML ()
tag t mHTML = do -- mHTML is a monadic argument
    string $ "<" ++ t ++ ">"
    mHTML
    string $ "</" ++ t ++ ">"

-- HTML a -> HTML ()
html  = tag "html"
headd  = tag "head"
title = tag "title"
body  = tag "body"
p     = tag "p"
i     = tag "i"
b     = tag "b"
h1    = tag "h1"
h2    = tag "h2"
h3    = tag "h3"
h4    = tag "h4"
ol    = tag "ol"
ul    = tag "ul"
table = tag "table"
tr    = tag "tr"
th    = tag "th"
td    = tag "td"
-- render (string "foo" >> string "bar")

doc =
    html $ do
        headd
         $ do
            title $ string "Hello, world!"
        body $ do
            h1 $ string "Greetings"
            p $ string "Hello, world!"

-- writeFile "hello.html" $ render doc writeFile "hello.html" $ render doc
-- Source http://cmsc-16100.cs.uchicago.edu/2016/Lectures/19-state-monad-2.php
