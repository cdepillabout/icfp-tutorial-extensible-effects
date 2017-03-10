{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- Extensible effects: a variation of Eff.hs emulating
-- monad transformer classes
-- It is possible to define the instances for MonadError,
-- MonadReader, MonadState etc. The upside: fewer type
-- annotations. The downside: loss of generality,
-- enforcing a single layer of a particular kind.

module ExtMTL where

import Control.Monad
import Data.Typeable
import OpenUnion1

import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Error (MonadError(..))
import Control.Monad.State (MonadState(..))

import qualified Eff as E
import Eff (Eff, Void, run, Reader, runReader, Exc, runError,
            Choose, choose, makeChoice, Trace, trace, runTrace, 
            State, runState,
            Yield, yield, runC, Y(..))


-- ------------------------------------------------------------------------
-- The Reader monad

instance (MemberU Reader (Reader e) r, Typeable e) => 
    MonadReader e (Eff r) where
    ask = E.ask
    local = E.local

-- Examples
add :: Monad m => m Int -> m Int -> m Int
add = liftM2 (+)

-- The type is inferred
t1 :: MonadReader Int m => m Int
t1 = ask `add` return 1


-- No need for annotation on the numeral 10
-- t1r :: Eff r Int
t1r = runReader t1 10

t1rr = run t1r
-- 11

-- The type is inferred
-- t3 :: Member (Reader Int) r => Eff r Int
-- t3 :: MonadReader Int m => m Int
t3 = t1 `add` local (+ (10::Int)) t1
t3r = run $ runReader t3 (100::Int)
-- 212

{-
t1rr' = run t1
    No instance for (MemberU Reader (Reader Int) Void)
      arising from a use of `t1'
-}

{- Cannot define this example: Reader layer is unique

-- Inferred type
-- t2 :: (Member (Reader Int) r, Member (Reader Float) r) => Eff r Float
t2 = do
  v1 <- ask
  v2 <- ask
  return $ fromIntegral (v1 + (1::Int)) + (v2 + (2::Float))

-- t2r :: Member (Reader Float) r => Eff r Float
t2r = runReader t2 (10::Int)
-- t2rr :: Eff r Float
t2rr = runReader (runReader t2 (10::Int)) (20::Float)

t2rrr = run t2rr
-- 33.0

-- The following example demonstrates true interleaving of Reader Int
-- and Reader Float layers
{-
t4
  :: (Member (Reader Int) r, Member (Reader Float) r) =>
     () -> Eff r Float
-}
t4 = liftM2 (+) (local (+ (10::Int)) t2) 
                (local (+ (30::Float)) t2)

t4rr = run $ runReader (runReader t4 (10::Int)) (20::Float)
-- 106.0
-- The opposite order of layers gives the same result
t4rr' = run $ runReader (runReader t4 (20::Float)) (10::Int)
-- 106.0
-}

-- Map an effectful function
-- The type is inferred
-- tmap :: Member (Reader Int) r => Eff r [Int]
tmap :: MonadReader Int m => m [Int]
tmap = mapM f [1..5]
 where f x = ask `add` return x

tmapr = run $ runReader tmap (10::Int)
-- [11,12,13,14,15]

-- ------------------------------------------------------------------------
-- Exceptions

instance (MemberU Exc (Exc e) r, Typeable e) => MonadError e (Eff r) where
    throwError = E.throwError
    catchError = E.catchError

-- The type is inferred
et1 :: Eff r Int
et1 = return 1 `add` return 2

et1r = run et1
-- 3

-- The type is inferred
-- et2 :: Member (Exc Int) r => Eff r Int
et2 :: MonadError Int m => m Int
et2 = return 1 `add` throwError (2::Int)

-- The following won't type: unhandled exception!
-- ex2r = run et2
{-
    No instance for (MemberU Exc (Exc Int) Void)
      arising from a use of `et2'
-}

-- The inferred type shows that ex21 is now pure
et21 :: Eff r (Either Int Int)
et21 = runError et2

et21r = run et21
-- Left 2

-- The example from the paper
newtype TooBig = TooBig Int deriving (Show, Typeable)
-- The type is inferred
-- ex2 :: Member (Exc TooBig) r => Eff r Int -> Eff r Int
ex2 :: MonadError TooBig m => m Int -> m Int
ex2 m = do
  v <- m
  if v > 5 then throwError (TooBig v)
     else return v


{-  No longer needed
-- specialization to tell the type of the exception
runErrBig :: Eff (Exc TooBig :> r) a -> Eff r (Either TooBig a)
runErrBig m = runError m
-}

-- The type is inferred; no need to annotate the numeral
-- ex2r :: Eff r (Either TooBig Int)
ex2r = runReader (runError (ex2 ask)) 5

ex2rr = run ex2r
-- Right 5

ex2rr1 = run $ runReader (runError (ex2 ask)) 7
-- Left (TooBig 7)

-- Different order of handlers (layers)
ex2rr2 = run $ runError (runReader (ex2 ask) 7)
-- Left (TooBig 7)

-- ------------------------------------------------------------------------
-- Combining exceptions and non-determinism

-- Example from the paper

ex2_2 = run . makeChoice . runError $ ex2 (choose [5,7,1])
-- [Right 5,Left (TooBig 7),Right 1]

-- just like ex1_1 in transf.hs but not at all like ex2_1 in transf.hs

-- with different order of handlers, obtain the desired result of
-- a high-priority exception
ex2_1 = run . runError . makeChoice $ ex2 (choose [5,7,1])
-- Left (TooBig 7)

-- Errror recovery part
-- The code is the same as in transf1.hs. The inferred signatures differ
-- Was: exRec :: MonadError TooBig m => m Int -> m Int
-- exRec :: Member (Exc TooBig) r => Eff r Int -> Eff r Int
exRec :: MonadError TooBig m => m Int -> m Int
exRec m = catchError m handler
 where handler (TooBig n) | n <= 7 = return n
       handler e = throwError e

ex2r_2 = run . runError . makeChoice $ exRec (ex2 (choose [5,7,1]))
-- Right [5,7,1]
-- Compare with ex2r_1 from transf1.hs

ex2r_1 = run . runError . makeChoice $ exRec (ex2 (choose [5,7,11,1]))
-- Left (TooBig 11)
-- Compare with ex2r_2 from transf1.hs


-- ------------------------------------------------------------------------
-- State

instance (Member (State s) r, Typeable s) => MonadState s (Eff r) where
    get = E.get
    put = E.put

-- Examples

ts1 :: MonadState Int m => m Int
ts1 = do
  put 10
  x <- get
  return x

ts1r = run (runState ts1 (0::Int))
-- (10,10)

ts2 :: MonadState Int m => m Int
ts2 = do 
  put 10
  x <- get
  put 20
  y <- get
  return (x+y) 

ts2r = run (runState ts2 (0::Int))
-- (30,20)

-- exceptions and state
incr :: MonadState Int m => m ()
incr = get >>= put . (+ 1)

tes1 :: (MonadState Int m,MonadError [Char] m) => m b
tes1 = do
 incr
 throwError "exc"

ter1 :: (Either String String, Int)
ter1 = run $ runState (runError tes1) (1::Int)
-- (Left "exc",2)

ter2 :: Either String (String, Int)
ter2 = run $ runError (runState tes1 (1::Int))
-- Left "exc"


teCatch :: MonadError String m => m a -> m [Char]
teCatch m = catchError (m >> return "done") return

ter3 :: (Either String String, Int)
ter3 = run $ runState (runError (teCatch tes1)) (1::Int)
-- (Right "exc",2)

ter4 :: Either String (String, Int)
ter4 = run $ runError (runState (teCatch tes1) (1::Int))
-- Right ("exc",2)


-- ------------------------------------------------------------------------
-- Co-routines

-- Add dynamic variables
-- The code is essentially the same as that in transf.hs (only added
-- a type specializtion on yield). The inferred signature is different though.
-- Before it was
--    th2 :: MonadReader Int m => CoT Int m ()
-- Now it is more general:
th2 :: (Member (Yield Int) r, MonadReader Int (Eff r)) => Eff r ()
th2 = ask >>= yield >> (ask >>= yield)


-- Code is essentially the same as in transf.hs; no liftIO though
-- Fewer type annotation
c2 = runTrace $ runReader (loop =<< runC th2) 10
 where loop (Y x k) = trace (show (x::Int)) >> k () >>= loop
       loop Done    = trace "Done"
{-
10
10
Done
-}

-- locally changing the dynamic environment for the suspension
c21 = runTrace $ runReader (loop =<< runC th2) 10
 where loop (Y x k) = trace (show (x::Int)) >> local (+1) (k ()) >>= loop
       loop Done    = trace "Done"
{-
10
11
Done
-}

-- Real example, with two sorts of local rebinding
-- th3 :: (Member (Yield Int) r, Member (Reader Int) r) => Eff r ()
-- th3 :: (Member (Yield Int) r, MonadReader Int (Eff r)) => Eff r ()
th3 :: (MemberU Reader (Reader Int) r, Member (Yield Int) r) =>
     Eff r ()
th3 = ay >> ay >> local (+10) (ay >> ay)
 where ay = ask >>= yield


c3 = runTrace $ runReader (loop =<< runC th3) 10
 where loop (Y x k) = trace (show (x::Int)) >> k () >>= loop
       loop Done    = trace "Done"
{-
10
10
20
20
Done
-}

-- locally changing the dynamic environment for the suspension
c31 = runTrace $ runReader (loop =<< runC th3) 10
 where loop (Y x k) = trace (show (x::Int)) >> local (+1) (k ()) >>= loop
       loop Done    = trace "Done"
{-
10
11
21
21
Done
-}
-- The result is exactly as expected and desired: the coroutine shares the
-- dynamic environment with its parent; however, when the environment
-- is locally rebound, it becomes private to coroutine.
