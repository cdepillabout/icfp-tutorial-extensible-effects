{-# LANGUAGE FlexibleContexts #-}

-- Needed nonly for defining an instance of MonadError
-- {-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}

-- Examples of various transformers, discussed in the paper

module TranEx where

import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.List
import Control.Monad.Cont
import Control.Monad.Identity

-- ========================================================================
-- Example 1: ErrorT and non-determinism

-- This example is quite like that in mtl-2.1.2 for the Reader monad
-- (with the abstact m instead of the concrete IO)
newtype TooBig = TooBig Int deriving Show
ex1 :: Monad m => m Int -> ErrorT TooBig m Int
ex1 m = do
  v <- lift m
  if v > 5 then throwError (TooBig v)
     else return v

instance Error TooBig

-- problems

ex1st = runState (runErrorT (ex1 (put 10 >> return 7))) 0
-- (Left (TooBig 7),10)

choose :: MonadPlus m => [a] -> m a
choose = msum . map return

ex1_1 = runIdentity . runListT . runErrorT  $ ex1 (choose [5,7,1])
-- [Right 5,Left (TooBig 7),Right 1]

-- What if we wanted to terminate the search?

-- A seemingly more flexible approach
ex2 :: MonadError TooBig m => m Int -> m Int
ex2 m = do
  v <- m
  if v > 5 then throwError (TooBig v)
     else return v

-- Now we get an exception
ex2_1 = runIdentity . runErrorT . runListT $ ex2 (choose [5,7,1])
-- Left (TooBig 7)

-- A different order of monads
ex2_2 = runIdentity . runListT . runErrorT $ ex2 (choose [5,7,1])
-- [Right 5]
-- The result is somewhat unexpected

exRec :: MonadError TooBig m => m Int -> m Int
exRec m = catchError m handler
 where handler (TooBig n) | n <= 7 = return n
       handler e = throwError e

ex2r_10 = runIdentity . runErrorT . runListT $ 
            exRec (ex2 (choose [5,7,1,11]))
-- Right [7]

ex2r_1 = runIdentity . runErrorT . runListT $ 
            exRec (ex2 (choose [5,7,1]))
-- Right [7]

-- ex2r1 by lifting the choices up
ex2r_15 = runIdentity . runErrorT . runListT $ 
            exRec (ex2 (return 5))
-- Right [5]
ex2r_17 = runIdentity . runErrorT . runListT $ 
            exRec (ex2 (return 7))
-- Right [7]
ex2r_11 = runIdentity . runErrorT . runListT $ 
            exRec (ex2 (return 1))
-- Right [1]

ex2r_2 = runIdentity . runListT . runErrorT $ 
            exRec (ex2 (choose [5,7,1,11]))
-- [Right 5]

  
-- inferred type
ex3 :: (Monad (t m), MonadTrans t, MonadError TooBig m) =>
     t m Int -> t m Int
ex3 m = do
  v <- m
  if v > 5 then lift $ throwError (TooBig v)
     else return v

ex3_1 = runIdentity . runErrorT . runListT $ 
         ex3 (choose [5,7,1])
-- Left (TooBig 7)


ex3_2 = runIdentity . runErrorT . runListT $ 
         exRec (ex3 (choose [5,7,1]))
-- Right [7]

-- abandoning all generality
ex3_rec1 :: ListT (ErrorT TooBig Identity) Int ->
             ListT (ErrorT TooBig Identity) Int
ex3_rec1 m = ListT $ catchError (runListT m) handler
 where handler (TooBig n) | n <= 7 = return [n]
       handler e = throwError e

ex3_rec11 = runIdentity . runErrorT . runListT $ 
             ex3_rec1 (ex3 (choose [5,7,1]))
-- Right [7]

-- Using two layers of exceptions
runErrorRelay :: MonadError e m => ErrorT e m a -> m a
runErrorRelay m = runErrorT m >>= check
 where check (Right x) = return x
       check (Left e)  = throwError e

ex4_1 = runIdentity . runErrorT . runListT . runErrorRelay $ 
        ex1 (choose [5,7,1])
-- Left (TooBig 7)

ex4_21 = runIdentity . runErrorT . runListT . runErrorRelay $ 
         exRec (ex1 (choose [5,7,1]))
-- Right [5,7,1]

ex4_22 = runIdentity . runErrorT . runListT . runErrorRelay $ 
         exRec (ex1 (choose [5,7,11,1]))
-- Left (TooBig 11)

-- A different way to non-determinism, using the proper monad transformer
-- this time
choose' :: Monad m => [a] -> ContT [a] m a
choose' lst = shift (\k -> liftM concat $ mapM k lst)

shift :: ((a -> m r) -> m r) -> ContT r m a
shift = ContT

runNDet :: Monad m => ContT [a] m a -> m [a]
runNDet m = runContT m (return . (:[]))

ex5_0 = runIdentity . runNDet $ (choose' [5,7,11,1])
-- [5,7,11,1]

{- Does not type check...
ex5_01 = runIdentity . runNDet . runErrorT $ 
         (ex1 (choose' [5,7,11,1]))

    Couldn't match expected type `Int'
                with actual type `Either TooBig Int'
    Expected type: ContT [Either TooBig Int] Identity Int
      Actual type: ContT
                     [Either TooBig Int] Identity (Either TooBig Int)
    In the return type of a call of choose'
    In the first argument of `ex1', namely `(choose' [5, 7, 11, 1])'
-}

{-
-- The following type-checks only if we define the MonadError instance
-- below
ex5_1 = runIdentity . runErrorT . runNDet . runErrorRelay $ 
        (ex1 (choose' [5,7,11,1]))
-- Left (TooBig 7)

instance MonadError e m => MonadError e (ContT r m) where
    throwError e = ContT $ \k -> throwError e
    catchError m handler = ContT $ \k -> 
                           catchError (runContT m k) 
                                      (\e -> runContT (handler e) k)

-- There if no other way to write catchError, but it is deficient

ce1 = runIdentity . runErrorT . (\m -> runContT m return) $
      throwError "err"
-- Left "err"

-- The print-out shows that the handler handed an exception that was raised
-- _after_ catchError!
ce2 :: IO (Either String ())
ce2 = runErrorT . (\m -> runContT m return) $
      catchError (return ()) handler  >> throwError "err"
 where
 handler e = liftIO . putStrLn $ "Handler: " ++ e
{-
Handler: err
Left "err"
-}
-}

-- We have to explicitly target the layer. What if m itself contains
-- the Cont monad? Then MonadError lifting will not work...
runErrorRelay' :: (Monad (t m), MonadTrans t, MonadError e m) =>
     ErrorT e (t m) b -> t m b
runErrorRelay' m = runErrorT m >>= check
 where check (Right x) = return x
       check (Left e)  = lift . throwError $ e

ex5_11 = runIdentity . runErrorT . runNDet . runErrorRelay' $ 
        (ex1 (choose' [5,7,11,1]))
-- Left (TooBig 7)

ex5_12 = runIdentity . runErrorT . runNDet . runErrorRelay' $ 
        (exRec (ex1 (choose' [5,7,1])))
-- Right [5,7,1]

ex5_13 = runIdentity . runErrorT . runNDet . runErrorRelay' $ 
        (exRec (ex1 (choose' [5,7,11,1])))
-- Left (TooBig 11)


ex5_14 :: IO (Either TooBig [Int])
ex5_14 = runErrorT . runNDet . runErrorRelay' $ 
        (exRec (lift (choose' [1,2]) >> ex1 (choose' [5,7,11]) >>= trace))
 where trace x = (liftIO . print) x >> return x

-- ========================================================================
-- Example 2: Simple coroutines (threads)

-- Status of a thread: done or reports the value of the type a
-- (For simplicity, a co-routine only reports a value but accepts unit)
data Y m a = Done | Y a (() -> m (Y m a))

type CoT a m = ContT (Y m a) m

-- Yield a value and suspend
yield :: Monad m => a -> CoT a m ()
yield x = shift (\k -> return $ Y x k)

-- Launch a thread and report its status
runC :: Monad m => CoT a m b -> m (Y m a)
runC m = runContT m (\_ -> return Done) 

-- First example
th1 :: Monad m => CoT Int m ()
th1 = yield 1 >> yield 2

c1 = loop =<< runC th1
 where loop (Y x k) = print x >> k () >>= loop
       loop Done    = print "Done"

{-
1
2
"Done"
-}

-- Add dynamic variables
th2 :: MonadReader Int m => CoT Int m ()
th2 = ask >>= yield >> (ask >>= yield)

c2 :: IO ()
c2 = runReaderT (loop =<< runC th2) 10
 where loop (Y x k) = liftIO (print x) >> k () >>= loop
       loop Done    = liftIO (print "Done")
{-
10
10
"Done"
-}

c21 :: IO ()
c21 = runReaderT (loop =<< runC th2) 10
 where loop (Y x k) = liftIO (print x) >> local (+1) (k ()) >>= loop
       loop Done    = liftIO (print "Done")

{-
10
11
"Done"
-}

-- Real example, with the Reader underneath CoT
th3 :: MonadReader Int m => CoT Int m ()
th3 = ay >> ay >> local (+10) (ay >> ay)
 where ay = ask >>= yield

-- Fail the local in th3
c3 :: IO ()
c3 = runReaderT (loop =<< runC th3) 10
 where loop (Y x k) = liftIO (print x) >> (k ()) >>= loop
       loop Done    = liftIO (print "Done")
{-
10
10
20
10
"Done"
-}

-- Both inner and outer locals have effect, but the local in th3
-- does not persist across yield. The overall result is weird.
c31 :: IO ()
c31 = runReaderT (loop =<< runC th3) 10
 where loop (Y x k) = liftIO (print x) >> local (+1) (k ()) >>= loop
       loop Done    = liftIO (print "Done")
{-
10
11
21
11
"Done"
-}

-- Reader over CoT
th4 :: Monad m => ReaderT Int (CoT Int m) ()
th4 = ay >> ay >> local (+10) (ay >> ay)
 where ay = ask >>= lift . yield

-- Now local in th4 acts as thread-local
c4 :: IO ()
c4 = loop =<< runC (runReaderT th4 10)
 where loop (Y x k) = liftIO (print x) >> (k ()) >>= loop
       loop Done    = liftIO (print "Done")

{-
10
10
20
20
"Done"
-}

{- But can't parameterize the thread, wrong type
c41 :: IO ()
c41 = loop =<< runC (runReaderT th4 10)
 where loop (Y x k) = liftIO (print x) >> local (+1) (k ()) >>= loop
       loop Done    = liftIO (print "Done")
-}

exst :: Monad m => (Int -> m Int) -> ListT m Int
exst m = do
  i <- return 5 `mplus` return 7
  lift $ m i

-- persistent state; may be undesirable
exst1 = runState (runListT (exst (\x -> modify (+x) >> get))) 0
-- ([5,12],12)

exst2 = runState (runListT (put 0 >> exst (\x -> modify (+x) >> get))) 0
-- ([5,12],12)

exl :: (MonadPlus m) => (Int -> m Int) -> m Int
exl m = do
  i <- return 5 `mplus` return 7
  m i

exl1 = runState (runListT (exl (\x -> modify (+x) >> get))) 0
-- ([5,12],12)

exl2 = runIdentity $ runListT (runStateT (exl (\x -> modify (+x) >> get)) 0)
-- [(5,5),(7,7)]

-- Adding trace/log
