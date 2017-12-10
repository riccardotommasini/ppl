module ESE20171205 where

import Control.Monad

newtype State st a = State {runState :: st -> (a, st)}

-- instance Functor (State s) where
-- 	fmap f (State g) = State ((\st -> 
-- 		let (a, st') = g st 
-- 		in (f a, st')))

instance Functor (State s) where
	fmap f ma = State $ (\(a,st) -> (f a, st)) . (runState ma)
									
instance Applicative (State s) where
	pure a =State $ (\st -> (a', st))
	(State ff) <*> (State fa) = State $ \s ->
					let (f, s') = ff s -- (+1) , "+1"
						(a, s'') = fa s' -- (0,"")
					in (f a, s'')


-- Monadic Club's Laws

-- 1) left identity a >>= f === f a
-- 2) right identity a >>= return === a
-- 3) associativity (a >>= f) >>= g === m >>= (\x f x >>= g)


instance Monad (State s) where
	return = pure
	(State a) >>= f = State $ \s ->
		let (a', s') = a s
			(State b) = f a'
			(b', s'') = b s'
		in (b', s'')

getState :: State state state
getState = State (\state -> (state, state))

putState :: state -> State state ()
putState new = State (\_ -> (new, ()))

mapListM :: (t -> State st a) -> [t] -> State st [a]
mapListM f [] = do return []
mapListM f (x:xs) = do x1 <- f x
					   xs1 <- mapListM f xs
					   return (x1:xs1) 

numList :: Num st => [st] -> State st [(st,st)]
numList list = mapListM helper list where
	helper x = do cur <- getState
			      putState (x+cur)
			      return (x, x+cur)




















