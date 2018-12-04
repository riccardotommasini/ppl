module State where

    import Control.Monad

    -- State Monad (Control.Monad.State)
        -- haskell pure functional nature saves functions to have side effects (i.e., modifying a global state).
        -- Function call just acts locally, computing and returning their results.
        -- But what if we need to actually modifying the global state (stateful computations)
        -- this need motivates the presence of the State Monad, that hides any edits to the global state, maintaining 
        -- the explicit computation purely functional.

    newtype State st a = State {runState :: st -> (a, st)}

    --  data can only be replaced with newtype if the type has exactly one 
        -- constructor with exactly one field inside it.

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


    -- Note the instance is State s, 
    -- and not just State: on its own, 
    -- State can't be made an instance of Monad, 
    -- as it takes two type parameters, rather than one. 
    -- That means there are actually many different State monads, 
    -- one for each possible type of state - 
    -- State String, State Int, State SomeLargeDataStructure, and so forth. 
    -- However, we only need to write one implementation of return and (>>=); 
    -- the methods will be able to deal with all choices of s.


    -- -- Monadic Functions to manipulate the state

    get :: State s s
    get = State $ \s -> (s, s)

    put :: s -> State s ()
    put t = State $ \s -> ((), t)

    edit :: (s -> s) -> State s ()
    edit f = State $ \s -> ((), f s)

    -- -- functions to access the internal state
    evalState :: State s a -> s -> a
    evalState ma s = fst (runState ma s)

    -- execState :: State s a -> s -> s
    -- execState ma s = snd (runState ma s)


    -- TE 2013/07/05

    -- Define the monadic function 
    -- mapListM :: (t -> State st a) -> [t] -> State st [a]
    -- that applies its first argument (another monadic function, as 
    -- you can see by the signature) to every element of the list
    -- passed as its second argument (i.e. like a map).

    mapListM :: (t -> State st a) -> [t] -> State st [a]
    mapListM f []     = do return []
    mapListM f (x:xs) = do x1 <- f x 
                           xs1 <- mapListM f xs -- recursive call
                           return (x1:xs1) -- wrap up a list

    -- considering :t (>>=), Monad m => m a -> (a -> m b) -> m b, 
    -- x1 is a, xs1 is [a]
    -- f x >>= (\x1 -> mapListM f xs >>= \xs1 -> return (x1:xs1) )
    -- f x is m a 
    -- the lambda is (\a -> m b)
    -- the m b is actually m [a]
    

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


    -- print runState $ numberList [1..10] 1


    -- Trees

    data Tree a = Leaf a | Branch ( Tree a ) ( Tree a )

    instance Show a => Show (Tree a) where
        show (Leaf a) = "Leaf" ++ show a
        show (Branch x y) = "< Left " ++ show x ++ " | Right" ++ show y ++ ">"
    
    --mapTreeM :: (t -> State st a) -> Tree t -> State st (Tree a)
    mapTreeM :: (Monad m) => (t -> m a) -> Tree t -> m (Tree a)
    mapTreeM f (Leaf a) = do
        b <- f a
        return (Leaf b)

    mapTreeM f (Branch lhs rhs) = do
        lhs' <- mapTreeM f lhs
        rhs' <- mapTreeM f rhs
        return (Branch lhs' rhs')

    numberTree :: Num st => Tree st -> State st (Tree (st, st))
    numberTree t = mapTreeM helper t where
        helper x = do st <- get
                      put (st+x) -- Changes?
                      return (x, st+x) -- changes?

    -- get >>= (\st -> put (st+x) >> \_ -> return (x, st+x))


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
