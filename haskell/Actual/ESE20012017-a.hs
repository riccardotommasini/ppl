module ESE20012017 where

    import qualified Data.Map as M
    --import Control.Monad.State

    -- k-v map monad
    type PersonName = String
    type PhoneNumber = Integer
    type BillingAddress = String
    data MobileCarrier = Honest_Bobs_Phone_Network
                       | Morrisas_Marvelous_Mobiles
                       | Petes_Plutocratic_Phones
                         deriving (Eq, Ord, Show)

    findCarrierBillingAddress :: PersonName
                              -> M.Map PersonName PhoneNumber
                              -> M.Map PhoneNumber MobileCarrier
                              -> M.Map MobileCarrier BillingAddress
                              -> Maybe BillingAddress

    -- Case of is a form of pattern matching (nothing more than standard function ). Right side have to share a common type
    findCarrierBillingAddress person phoneMap carrierMap addressMap =
        case M.lookup person phoneMap of
          Nothing -> Nothing -- case one
          Just number -> -- case two 
              case M.lookup number carrierMap of
                Nothing -> Nothing
                Just carrier -> M.lookup carrier addressMap

    findCarrierBillingAddress2 :: PersonName
                              -> M.Map PersonName PhoneNumber
                              -> M.Map PhoneNumber MobileCarrier
                              -> M.Map MobileCarrier BillingAddress
                              -> Maybe BillingAddress

    -- Now we exploit the monadic action using do notation
    findCarrierBillingAddress2 person phoneMap carrierMap addressMap = do
        number <- M.lookup person phoneMap
        carrier <- M.lookup number carrierMap
        a <- M.lookup carrier addressMap
        return a

    --USAGE
    pm = M.fromList [("ric", 331), ("mat", 335), ("stud", 347)]
    cm = M.fromList [(331, Honest_Bobs_Phone_Network), (335, Morrisas_Marvelous_Mobiles ), (347, Petes_Plutocratic_Phones)]
    am = M.fromList [(Honest_Bobs_Phone_Network, "via"), (Morrisas_Marvelous_Mobiles, "piazza" ), (Petes_Plutocratic_Phones, "angolo")]
    

    -- State Monad (Control.Monad.State)
    -- haskell pure functional nature saves functions to have side effects (i.e., modifying a global state).
    -- Function call just acts locally, computing and returning their results.
    -- But what if we need to actually modifying the global state (stateful computations)
    -- this need motivates the presence of the State Monad, that hides any edits to the global state, maintaining 
    -- the explicit computation purely functional.

    newtype State s a = State { runState :: s -> (a, s) }  

    -- stateful computation that manipulates a state of type s resulting in a type a

    -- the Monad typeclass wants a constructor with a single value like Maybe a. 
    -- State s a has two values, so we fix s as a part of the constructor
    
    instance Functor (State s) where
        fmap f st = let 
                    fstate = runState st
                    in State (\x -> let (a,b) = fstate x in (f a, b)) 
            
    instance Applicative (State s) where
        pure x = State $ \s -> (x,s)  -- x is a value
        stateFun <*> stateDat = let 
                        fstateFun = runState stateFun
                        fstateDat = runState stateDat
                in State (\x -> let 
                                    (f, _) = fstateFun x
                                    (a, s) = fstateDat x
                                in (f a, s))

    instance Monad (State s) where  
        -- return is pure
        m >>= f = State $ \s -> let (a, s') = runState m s  -- s is the state, h is a stateful computation that returns a pair (newVal, newState)    
                                        in  runState (f a) s'
    -- -- -- RETURN: the minimal monadic context is represented by State s x, i.e., it returns just x without changing the state
    -- -- >>=: the lambda is our stateful computation.
    -- -- h is 
    
    data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Eq)

    treeMapM :: (a -> State s b) -> Tree a -> State s (Tree b)
    treeMapM _ EmptyTree = return EmptyTree
    treeMapM f (Node a l r) = do
        n' <- f a
        l' <- treeMapM f l
        r' <- treeMapM f r
        return (Node n' l' r')


    get :: State s s
    get =  State $ \s -> (s,s)

    put :: s -> State s ()
    put s = State $ \_ -> ((), s) -- ignoring is important since the state must be immutable out these methods

    -- collectTree :: Tree b -> State [b] (Tree b)
    -- collectTree tree = treeMapM collectList tree
    --      where collectList v = do
    --             cur <- get
    --             put ( [v] )
    --             return v






