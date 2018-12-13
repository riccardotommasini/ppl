module ESE20181213 where
    import Control.Monad


   type Log = [String]

   newtype Logger a = Logger { run :: (a, Log)}

   instance (Show a) => Show (Logger a) where
   		show (Logger a) = show a


   instance (Eq a) => Eq (Logger a) where
   		Logger (x,y) /= Logger (a,b) = (x /= a) || (y /= b)


   logmapp :: (a -> b) -> Logger a -> Logger b
   logmapp f log =
   		let (a,l) = run log
   			b = f a
   		in Logger (b, l)


   instance Functor Logger where
   		fmap = logmapp

   logbind :: Logger (a -> b) -> Logger a -> Logger b
   logbind lf lv =
   		let (f, llf) = run lf
   			(b, llf') = 


   instance Applicative Logger where
   		pure a = Logger (a, [])
   		(<*>) =. logbind





