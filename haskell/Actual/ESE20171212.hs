module ESE20171212 where

import Control.Applicative
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

data Tree a = Leaf a | Branch (Tree a) (Tree a)

instance Show a => Show (Tree a) where
    show (Leaf a) = show a
    show (Branch l r) = "<" ++ show l ++ " | " ++ show r ++ ">"


numberTree  :: Tree a -> Tree (a, Int)
numberTree t = fst ( numberTree' t 0 ) where
    numberTree' (Leaf a) st = (Leaf (a, st), st+1)
    numberTree' (Branch l r) st =
        let (l', st') = numberTree' l st
            (r', st'') = numberTree' r st'
        in (Branch l' r', st'')


put :: s -> State s ()
put new = State (\_ -> ((), new))

get :: State state state
get = State (\state -> (state, state))

runStateM :: State state a -> state -> a
runStateM (State f) st = fst (f st)

mapTreeM :: (Monad m) => (t -> m a) -> Tree t -> m (Tree a)
mapTreeM f (Leaf t) = do
        a <- f t
        return (Leaf a)
mapTreeM f (Branch l r) = do
        l' <- mapTreeM f l
        r' <- mapTreeM f r
        return (Branch l' r')


numberTreeM :: Tree a -> State Int (Tree (a, Int))
numberTreeM tree = mapTreeM number tree 
    where number value = get >>= 
                        (\cur -> put (cur+1) >>=
                            (\_ -> return (value, cur)))


 -- do
 --            c <- get
 --            put (c+1)
 --            return (value,c+1)

-- LOGGER


type Log = [String]

newtype Logger a = Logger {run :: (a, Log)}

instance (Show a) => Show (Logger a) where
    show (Logger a) = show a

instance (Eq a) => Eq (Logger a) where
    Logger (x,y) /= Logger (a,b) = (x /= a) || (y /= b)

instance Functor Logger where
    fmap f log = 
        let (a,ls) = run log
            b = f a
        in Logger (b, ls)

llap lf la = 
    let (f, lsf) = run lf
        ln = fmap f la
        (b, l) = run ln
    in Logger (b, l ++ lsf)


instance Applicative Logger where
    pure a = Logger (a, [])
    (<*>) = llap 


instance Monad Logger where
    return = pure
    m >>= f =
        let (a,l) = run m
            m' = f a
            (b, l') = run m'
        in Logger (b, l ++ l')





logPlus1 :: (Num a) => a -> Logger a
logPlus1 a = Logger (a+1, ["+1"])

multiplyBy2 :: (Num a) => a -> Logger a
multiplyBy2 a = Logger (a*2, ["*2"])


main logger = do
    v <- logger
    p1 <- logPlus1 v
    m2 <- multiplyBy2 p1
    return m2

main2 logger = 
        logger >>= (\v -> logPlus1 v >>= 
        (\p1 -> multiplyBy2 p1 >>= 
        (\m2 -> return m2)))

-- e.g. let t = Branch (Branch (Leaf 'a') (Branch (Leaf 'b') (Leaf 'c'))) (Branch (Leaf 'd') (Leaf 'e'))
-- runStateM (numberTreeM t) 1
-- numberTree t






