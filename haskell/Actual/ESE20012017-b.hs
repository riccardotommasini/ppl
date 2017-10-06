module ESE20012017 where

import Control.Monad.State

-- newtype State s a = State { runState :: s -> (a,s)}

-- instance Monad State where
--  return x = State $ \s -> (x,s)
--  m >>= f = State $ \s -> let 
--                          fs_of_m = (runState m)
--                          (a, s') =  fs_of_m s
--                          m' = f a 
--                          fs_of_m' = (runState m')
--                          in  fs_of_m' s'

-- instance Monad (State s) where  
--  return x = State $ \s -> (x,s)  
--  (State h) >>= f = State $ \s -> let (a, newState) = h s  
--                                     (State g ) = f a  -- ta -> State ts ta
--                                 in  g newState

type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x,xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a:xs)

stackManip :: Stack -> (Int, Stack)
stackManip stack = let
        (a, stack') = pop stack
        (b, stack'') = pop stack'
        ((), stack''')= push 100 stack''
        in pop stack'''

-- get :: State s s
-- get = Stata $ \s -> (s,s)

-- put:: s -> State s ()
-- put s = State $ \_ -> ((),s)

popM :: State Stack Int
popM = do
    head:stack <- get 
    put stack
    return head

pushM :: Int -> State Stack () 
pushM a = do
    stack <- get
    put (a:stack)
    return ()


stackManipM = do
    popM
    popM
    pushM 100
    popM
    pushM 101


runState stackManipM [1,2,3,4]








