module ESE20181204 where
    import Control.Monad

    newtype State st a = State {runState :: st -> (a, st)}

    -- instance Functor (State s) where
    --     fmap f (State g) =  
    --         State (\st -> let (x, s) = g st 
    --             in (f x, s))

    instance Functor (State s) where
        fmap f ma = State $ (\(a,s) -> (f a, s)) . runState ma


    instance Applicative (State s) where
        pure a = State (\s -> (a, s))
        (State f) <*> (State a) = State $ \s ->
                        let (f', s') = f s
                            (f'', s'') = a s'
                        in (f' f'', s'')


    -- Left Identity  a >>= f === f a
    -- Right Identity m >>= return === return m
    -- Associativity (m >>= f) >>= g === m >>= (\x -> f x >>= g)

    instance Monad (State s) where
        return = pure
        (State a) >>= f = State $ \s ->
                    let (a', s')  = a s
                        (State b) = f a'
                        (b', s'') = b s'
                    in (b', s'')

    -- 
    get :: State s s
    get = State $ \s -> (s, s)

    put :: s -> State s ()
    put t = State $ \s -> ((), t)

    edit :: (s -> s) -> State s ()
    edit f = State $ \s -> ((), f s)


-- TE 2013/07/05

-- Define the monadic function 
-- mapListM :: (t -> State st a) -> [t] -> State st [a]
-- that applies its first argument (another monadic function, as 
-- you can see by the signature) to every element of the list
-- passed as its second argument (i.e. like a map).

    mapListM :: (t -> State st a) -> [t] -> State st [a]
    mapListM f [] = do return []
    mapListM f (x:xs) = do x1 <- f x
                           xs1 <- mapListM f xs
                           return (x1:xs1)

-- mapListM done with lambdas instead of do notation
    mapListM2 :: (t -> State st a) -> [t] -> State st [a]
    mapListM2 f [] = State(\s -> ([], s))
    mapListM2 f (x:xs) = State runState where
            runState s = ((a:as), s'') where
                (State rs1) = f x
                (State rs2) = (mapListM2 f xs)
                (a, s') = rs1 s
                (as, s'') = rs2 s'


-- Define the monadic function 
-- numberList :: Num st => [st] -> State st [(st, st)],
-- **based on mapListM**, that takes as input a 
-- list of numbers and returns a list of pairs of 
-- numbers (x, y), where the first component is 
-- the same as the value x at the same position 
-- in the input list, while y is the state when x 
-- was reached. The state is incremented by x, 
-- when x is reached.

    numberList :: (Num st) => [st] -> State st [(st, st)]
    numberList l = mapListM helper l where
        helper x = do st <- get
                      put (x + st)
                      return (x, x+st)

-- numberList done with lambdas instead of do notation
    numberList2 :: (Num st) => [st] -> State st [(st, st)]
    numberList2 l = mapListM helper l where
        helper x = State(\s -> ((x, s+x), s+x))

-- numberList done using the lambda version of the mapListM function (for testing purposes)
    numberList3 :: (Num st) => [st] -> State st [(st, st)]
    numberList3 l = mapListM2 helper l where
        helper x = State(\s -> ((x, s+x), s+x))

-- Additional helper function useful for debugging and printing
    doComputations :: (State st a) -> st -> (a, st)
    doComputations (State runState) initialState = runState initialState
        
-- TEST: 
--        doComputations (numberList [1,2,3,4,5]) 0
--        doComputations (numberList2 [1,2,3,4,5]) 0
--        doComputations (numberList3 [1,2,3,4,5]) 0
--          should all yield as a result    ([(1,1),(2,3),(3,6),(4,10),(5,15)],15)

    data Tree a = Leaf a | Branch ( Tree a ) ( Tree a )

    instance Show a => Show (Tree a) where
        show (Leaf a) = show a
        show (Branch x y) = "<" ++ show x ++ " | " ++ show y ++ ">"

    -- homework

    -- mapTreeM :: (t -> State st a) -> Tree t -> State st (Tree t)



