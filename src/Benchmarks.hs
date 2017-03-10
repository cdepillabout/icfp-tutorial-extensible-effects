{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

-- Benchmarks of the Eff framework of extensible effects
--
-- ghc -O2 --make Benchmarks.hs
-- or
-- ghc -O2 -rtsopts -main-is Benchmarks.mainCnt_State Benchmarks.hs
-- To run this code
-- GHCRTS="-tstderr" ./Benchmarks

module Benchmarks where

import Eff1 as E
import OpenUnion51 (Member)
import Control.Monad

-- For comparison
-- We use a strict State monad, because of large space leaks with the
-- lazy monad (one test even overflows the stack)
import Control.Monad.State.Strict as S
import Control.Monad.Error  as Er
import Control.Monad.Reader as Rd
import Control.Monad.Cont as Ct

-- ------------------------------------------------------------------------
-- Single State, with very little non-effectful computation
-- This is a micro-benchmark, and hence not particularly realistic.
-- Because of its simplicity, GHC may do a lot of inlining.
-- See a more realistic max benchmark below, which does a fair amount
-- of computation other than accessing the state.

-- Count-down
benchCnt_State :: Int -> ((),Int)
benchCnt_State n = S.runState m n 
 where
 m = do
     x <- S.get
     if x > 0 then S.put (x-1) >> m else return ()


mainCnt_State = print $ benchCnt_State 10000000
-- bytecode Benchmarks:
-- ((),0)
-- (10.41 secs, 4322528400 bytes)
{-
-- Compiled Benchmarks:
((),0)
<<ghc: 160051336 bytes, 306 GCs, 28400/28400 avg/max bytes residency (1 samples), 1M in use, 0.00 INIT (0.00 elapsed), 0.01 MUT (0.02 elapsed), 0.00 GC (0.00 elapsed) :ghc>>
((),0)

<<ghc: 160050928 bytes, 306 GCs, 36364/44312 avg/max bytes residency (2 samples), 1M in use, 0.00 INIT (0.00 elapsed), 0.02 MUT (0.02 elapsed), 0.00 GC (0.00 elapsed) :ghc>>

-}

benchCnt_Eff :: Int -> ((),Int)
benchCnt_Eff n = run $ E.runState m n 
 where
 m = do
     x <- E.get
     if x > 0 then E.put (x-1::Int) >> m else return ()

mainCnt_Eff = print $ benchCnt_Eff 10000000
-- bytecode Benchmarks:
-- Eff original
-- ((),0)
-- (18.58 secs, 16560000576 bytes) 
{-
-- Compiled Benchmarks:
((),0)
<<ghc: 2080052304 bytes, 4112 GCs, 28488/28488 avg/max bytes residency (1 samples), 1M in use, 0.00 INIT (0.00 elapsed), 0.75 MUT (0.77 elapsed), 0.02 GC (0.02 elapsed) :ghc>>
Eff1, FTCQueue
((),0)
<<ghc: 2160051032 bytes, 4156 GCs, 36376/44312 avg/max bytes residency (2 samples), 1M in use, 0.00 INIT (0.00 elapsed), 1.19 MUT (1.19 elapsed), 0.01 GC (0.02 elapsed) :ghc>>

Eff1, FTCQueue1
((),0)
<<ghc: 1520051008 bytes, 2916 GCs, 36364/44312 avg/max bytes residency (2 samples), 1M in use, 0.00 INIT (0.00 elapsed), 1.14 MUT (1.14 elapsed), 0.01 GC (0.01 elapsed) :ghc>>
without strictness annotations
((),0)
<<ghc: 1520051008 bytes, 2916 GCs, 36364/44312 avg/max bytes residency (2 samples), 1M in use, 0.00 INIT (0.00 elapsed), 1.08 MUT (1.08 elapsed), 0.01 GC (0.01 elapsed) :ghc>>

OpenUnion51, various inlinings... No-full-laziness hurts!
   (allocates more memory)
((),0)
<<ghc: 1600051008 bytes, 3084 GCs, 36364/44312 avg/max bytes residency (2 samples), 1M in use, 0.00 INIT (0.00 elapsed), 1.19 MUT (1.20 elapsed), 0.01 GC (0.01 elapsed) :ghc>>

Using re-writing rules for get/put, to expose the send/bind rule
Speed-up by the factor of 2.
((),0)
<<ghc: 800050968 bytes, 1532 GCs, 36392/44312 avg/max bytes residency (2 samples), 1M in use, 0.00 INIT (0.00 elapsed), 0.49 MUT (0.49 elapsed), 0.01 GC (0.01 elapsed) :ghc>>
-}
-- Eff1, FastTCQueue (all interepreted)
{-
*Benchmarks> print $ benchCnt_Eff 10000
((),0)
(0.35 secs, 51871360 bytes)
*Benchmarks> print $ benchCnt_Eff 100000
((),0)
(3.31 secs, 880860904 bytes)
*Benchmarks> print $ benchCnt_Eff 200000
((),0)
(6.62 secs, 1761784920 bytes)
*Benchmarks> print $ benchCnt_Eff 400000
((),0)
(13.21 secs, 3524051224 bytes)
-}

-- Eff1, FTCQueue (all interepreted)
-- (seems twice as fast)
{-
*Benchmarks> print $ benchCnt_Eff 10000
((),0)
(0.24 secs, 55411656 bytes)
*Benchmarks> print $ benchCnt_Eff 100000
((),0)
(1.88 secs, 426665128 bytes)
*Benchmarks> print $ benchCnt_Eff 200000
((),0)
(3.72 secs, 905822056 bytes)
*Benchmarks> print $ benchCnt_Eff 400000
((),0)
(7.28 secs, 1813115696 bytes)
*Benchmarks> print $ benchCnt_Eff 800000
((),0)
(14.58 secs, 3626175776 bytes)
-}


-- ------------------------------------------------------------------------
-- Single Error
-- Multiply a list of numbers, throwing an exception when encountering 0
-- This is again a mcro-benchmark

-- make a list of n ones followed by 0
be_make_list :: Int -> [Int]
be_make_list n = replicate n 1 ++ [0]

mainMul_pure = print . product $ be_make_list 1000000
-- 0
-- (0.36 secs, 201559304 bytes)
{-
0
<<ghc: 48050696 bytes, 92 GCs, 28400/28400 avg/max bytes residency (1 samples), 1M in use, 0.00 INIT (0.00 elapsed), 0.01 MUT (0.01 elapsed), 0.00 GC (0.00 elapsed) :ghc>>
0
-}

instance Error Int where

benchMul_Error :: Int -> Int
benchMul_Error n = either id id m
 where
 m = foldM f 1 (be_make_list n)
 f acc 0 = Er.throwError 0
 f acc x = return $! acc * x

mainMul_Error = print $ benchMul_Error 1000000
-- 0
-- (1.39 secs, 584028840 bytes)
{-
0
<<ghc: 160050776 bytes, 307 GCs, 28432/28432 avg/max bytes residency (1 samples), 1M in use, 0.00 INIT (0.00 elapsed), 0.03 MUT (0.03 elapsed), 0.00 GC (0.00 elapsed) :ghc>>
0
-}

benchMul_Eff :: Int -> Int
benchMul_Eff n = either id id . run . runError $ m
 where
 m = foldM f 1 (be_make_list n)
 f acc 0 = E.throwError (0::Int)
 f acc x = return $! acc * x

mainMul_Eff = print $ benchMul_Eff 1000000
-- 0
-- (1.09 secs, 519988392 bytes)
{-
0
<<ghc: 248052688 bytes, 474 GCs, 28432/28432 avg/max bytes residency (1 samples), 1M in use, 0.00 INIT (0.00 elapsed), 0.07 MUT (0.06 elapsed), 0.00 GC (0.00 elapsed) :ghc>>
-}


-- ------------------------------------------------------------------------
-- Investigating the effect of adding layers of effects
-- We start with a basic State computation and add leyers of dummy
-- Reader effects.

-- A single State computation, with the possibility to add more layers
-- We have to use the Strict State monad and add strictness annotations.
-- Otherwise, the byte code runs twice as slow and the compiled
-- code overflows the stack.
benchS_MTL :: (MonadState Integer m) => Integer -> m Integer
benchS_MTL n = foldM f 1 [n, n-1 .. 0]
 where
 f acc x | x `mod` 5 == 0 = do
                            s <- S.get 
                            S.put $! (s+1)
                            return $! max acc x
 f acc x = return $! max acc x
mainS_MTL = print $ S.runState (benchS_MTL 10000000) 0
-- bytecode
-- (10000000,2000001)
-- (32.02 secs, 11649331752 bytes)
{-
Compiled
(10000000,2000001)
<<ghc: 3008052064 bytes, 5757 GCs, 28576/28576 avg/max bytes residency (1 samples), 1M in use, 0.00 INIT (0.00 elapsed), 1.17 MUT (1.18 elapsed), 0.03 GC (0.03 elapsed) :ghc>>
-}


mainRS_MTL = print $ 
  flip Rd.runReader (0::Int) $ 
    S.runStateT (benchS_MTL 10000000) 0
-- (10000000,2000001)
-- (33.45 secs, 13665229776 bytes)
{-
(10000000,2000001)
<<ghc: 3728052136 bytes, 7134 GCs, 28568/28568 avg/max bytes residency (1 samples), 1M in use, 0.00 INIT (0.00 elapsed), 1.28 MUT (1.30 elapsed), 0.03 GC (0.03 elapsed) :ghc>>
-}

mainRRS_MTL = print $ 
  flip Rd.runReader (0::Int) $ 
  flip Rd.runReaderT (0::Integer) $ 
    S.runStateT (benchS_MTL 10000000) 0
-- (10000000,2000001)
-- (35.42 secs, 15681462456 bytes)
{-
(10000000,2000001)
<<ghc: 4768052240 bytes, 9140 GCs, 28472/28472 avg/max bytes residency (1 samples), 1M in use, 0.00 INIT (0.00 elapsed), 1.52 MUT (1.55 elapsed), 0.04 GC (0.04 elapsed) :ghc>>
-}

mainRRRS_MTL = print $ 
  flip Rd.runReader (0::Int) $ 
  flip Rd.runReaderT (0::Integer) $ 
  flip Rd.runReaderT True $ 
    S.runStateT (benchS_MTL 10000000) 0
-- (10000000,2000001)
-- (36.49 secs, 17695985712 bytes)
{-
(10000000,2000001)
<<ghc: 5968052360 bytes, 11459 GCs, 28600/28600 avg/max bytes residency (1 samples), 1M in use, 0.00 INIT (0.00 elapsed), 1.76 MUT (1.79 elapsed), 0.05 GC (0.05 elapsed) :ghc>>
-}

mainRRRRS_MTL = print $ 
  flip Rd.runReader (0::Int) $ 
  flip Rd.runReaderT (0::Integer) $ 
  flip Rd.runReaderT True $ 
  flip Rd.runReaderT "0" $ 
    S.runStateT (benchS_MTL 10000000) 0
-- (10000000,2000001)
-- (37.32 secs, 19711882088 bytes)
{-
(10000000,2000001)
<<ghc: 7328052496 bytes, 14063 GCs, 28632/28632 avg/max bytes residency (1 samples), 1M in use, 0.00 INIT (0.00 elapsed), 2.07 MUT (2.17 elapsed), 0.06 GC (0.07 elapsed) :ghc>>
-}



-- This time, adding Reader layers underneath the State
mainSR_MTL = print $ 
    flip S.runState 0 $
     flip Rd.runReaderT (0::Int) $ 
      (benchS_MTL 10000000)
-- (10000000,2000001)
-- (33.70 secs, 13617816624 bytes)
{-
(10000000,2000001)
<<ghc: 3808052144 bytes, 7292 GCs, 28568/28568 avg/max bytes residency (1 samples), 1M in use, 0.00 INIT (0.00 elapsed), 1.28 MUT (1.32 elapsed), 0.03 GC (0.03 elapsed) :ghc>>
-}

mainSRR_MTL = print $ 
    flip S.runState 0 $
     flip Rd.runReaderT (0::Int) $ 
     flip Rd.runReaderT (0::Integer) $ 
      (benchS_MTL 10000000)
-- (10000000,2000001)
-- (35.36 secs, 15538349728 bytes)
{-
(10000000,2000001)
<<ghc: 4928052256 bytes, 9448 GCs, 28472/28472 avg/max bytes residency (1 samples), 1M in use, 0.00 INIT (0.00 elapsed), 1.55 MUT (1.58 elapsed), 0.04 GC (0.04 elapsed) :ghc>>
-}

mainSRRR_MTL = print $ 
    flip S.runState 0 $
     flip Rd.runReaderT (0::Int) $ 
     flip Rd.runReaderT (0::Integer) $ 
     flip Rd.runReaderT True $ 
      (benchS_MTL 10000000)
-- (10000000,2000001)
-- (36.01 secs, 17456368112 bytes)
{-
(10000000,2000001)
<<ghc: 6208052384 bytes, 11905 GCs, 28552/28552 avg/max bytes residency (1 samples), 1M in use, 0.00 INIT (0.00 elapsed), 1.80 MUT (1.83 elapsed), 0.05 GC (0.05 elapsed) :ghc>>
-}

mainSRRRR_MTL = print $ 
    flip S.runState 0 $
     flip Rd.runReaderT (0::Int) $ 
     flip Rd.runReaderT (0::Integer) $ 
     flip Rd.runReaderT True $ 
     flip Rd.runReaderT "0" $ 
      (benchS_MTL 10000000)
-- (10000000,2000001)
-- (37.25 secs, 19376003040 bytes)
{-
(10000000,2000001)
<<ghc: 7648052528 bytes, 14669 GCs, 28720/28720 avg/max bytes residency (1 samples), 1M in use, 0.00 INIT (0.00 elapsed), 2.19 MUT (2.23 elapsed), 0.07 GC (0.07 elapsed) :ghc>>
-}

-- Conclusion: adding a new Reader layer adds 1 second to the running time
-- in bytecode and just as steadily adds to running time in the compiled code.
-- The effect is observed whether we add new Reader layers over the State
-- or under the State.


-- benchS_MTL re-written for the Eff monad
benchS_Eff :: (Member (E.State Integer) r) =>
                Integer -> Eff r Integer
benchS_Eff n = foldM f 1 [n, n-1 .. 0]
 where
 f acc x | x `mod` 5 == 0 = do
                            s <- E.get 
                            E.put $! (s+1::Integer)
                            return $! max acc x
 f acc x = return $! max acc x

mainS_Eff = print $ 
 run $ E.runState (benchS_Eff 10000000) (0::Integer)
-- (10000000,2000001)
-- (34.38 secs, 15042586288 bytes)
{-
(10000000,2000001)
<<ghc: 5632055448 bytes, 10001 GCs, 29040/29040 avg/max bytes residency (1 samples), 1M in use, 0.00 INIT (0.00 elapsed), 2.62 MUT (2.67 elapsed), 0.06 GC (0.06 elapsed) :ghc>>
-}

mainRS_Eff = print $ run $ 
  flip E.runReader (0::Int) $
   E.runState (benchS_Eff 10000000) (0::Integer)
-- (10000000,2000001)
-- (34.07 secs, 15043052808 bytes)
{-
(10000000,2000001)
<<ghc: 5632055512 bytes, 10001 GCs, 29072/29072 avg/max bytes residency (1 samples), 1M in use, 0.00 INIT (0.00 elapsed), 2.60 MUT (2.64 elapsed), 0.06 GC (0.06 elapsed) :ghc>>
-}

mainRRS_Eff = print $ run $ 
  flip E.runReader (0::Int) $
  flip E.runReader (0::Integer) $
   E.runState (benchS_Eff 10000000) (0::Integer)
-- (10000000,2000001)
-- (34.27 secs, 15039869104 bytes)
{-
(10000000,2000001)
<<ghc: 5632055616 bytes, 10001 GCs, 29112/29112 avg/max bytes residency (1 samples), 1M in use, 0.00 INIT (0.00 elapsed), 2.59 MUT (2.66 elapsed), 0.06 GC (0.06 elapsed) :ghc>>
-}

mainRRRS_Eff = print $ run $ 
  flip E.runReader (0::Int) $
  flip E.runReader (0::Integer) $
  flip E.runReader True $
   E.runState (benchS_Eff 10000000) (0::Integer)
-- (10000000,2000001)
-- (33.93 secs, 15039870120 bytes)
{-
(10000000,2000001)
<<ghc: 5632055720 bytes, 10001 GCs, 29152/29152 avg/max bytes residency (1 samples), 1M in use, 0.00 INIT (0.00 elapsed), 2.56 MUT (2.60 elapsed), 0.06 GC (0.06 elapsed) :ghc>>
(10000000,2000001)
-}

mainRRRRS_Eff = print $ run $ 
  flip E.runReader (0::Int) $
  flip E.runReader (0::Integer) $
  flip E.runReader True $
  flip E.runReader "0" $
   E.runState (benchS_Eff 10000000) (0::Integer)
-- (10000000,2000001)
-- (33.89 secs, 15039869848 bytes)
{-
(10000000,2000001)
<<ghc: 5632055824 bytes, 10001 GCs, 29192/29192 avg/max bytes residency (1 samples), 1M in use, 0.00 INIT (0.00 elapsed), 2.65 MUT (2.68 elapsed), 0.06 GC (0.06 elapsed) :ghc>>
-}

mainSR_Eff = print $ run $ 
  flip E.runState (0::Integer) $
  flip E.runReader (0::Int) $
   benchS_Eff 10000000
-- (10000000,2000001)
-- (34.99 secs, 16003326472 bytes)
{-
(10000000,2000001)
<<ghc: 6592056888 bytes, 11905 GCs, 29128/29128 avg/max bytes residency (1 samples), 1M in use, 0.00 INIT (0.00 elapsed), 2.94 MUT (3.02 elapsed), 0.07 GC (0.07 elapsed) :ghc>>
-}

mainSRR_Eff = print $ run $ 
  flip E.runState (0::Integer) $
  flip E.runReader (0::Int) $
  flip E.runReader (0::Integer) $
   benchS_Eff 10000000
-- (10000000,2000001)
-- (35.81 secs, 16959985920 bytes)
{-
(10000000,2000001)
<<ghc: 7552058416 bytes, 13699 GCs, 29176/29176 avg/max bytes residency (1 samples), 1M in use, 0.00 INIT (0.00 elapsed), 3.50 MUT (3.56 elapsed), 0.08 GC (0.08 elapsed) :ghc>>
-}

mainSRRR_Eff = print $ run $ 
  flip E.runState (0::Integer) $
  flip E.runReader (0::Int) $
  flip E.runReader (0::Integer) $
  flip E.runReader True $
   benchS_Eff 10000000
-- (10000000,2000001)
-- (35.60 secs, 17920031600 bytes)
{-
(10000000,2000001)
<<ghc: 8512059944 bytes, 15626 GCs, 29224/29224 avg/max bytes residency (1 samples), 1M in use, 0.00 INIT (0.00 elapsed), 3.67 MUT (3.70 elapsed), 0.09 GC (0.09 elapsed) :ghc>>
-}

mainSRRRR_Eff = print $ run $ 
  flip E.runState (0::Integer) $
  flip E.runReader (0::Int) $
  flip E.runReader (0::Integer) $
  flip E.runReader True $
  flip E.runReader "0" $
   benchS_Eff 10000000
-- (10000000,2000001)
-- (36.18 secs, 18880009784 bytes)
{-
(10000000,2000001)
<<ghc: 9472095120 bytes, 17392 GCs, 61128/61128 avg/max bytes residency (1 samples), 1M in use, 0.00 INIT (0.00 elapsed), 3.93 MUT (3.99 elapsed), 0.10 GC (0.11 elapsed) :ghc>>
-}

-- Conclusion: adding a new Reader effect may influence the timing, depending
-- on the order of the handlers. On one order, there is no effect
-- of more handlers on performance. Both the running time and allocated
-- memory stay constant. When the State handler is the last, we see the effect
-- of the relaying the execption. The effect is less steady as with MTL
-- (especially in bytecode).


-- ------------------------------------------------------------------------
-- State and Error and non-effectful computation

benchMax_MTL :: (MonadState Int m, MonadError Int m) => Int -> m Int
benchMax_MTL n = foldM f 1 [n, n-1 .. 0]
 where
 f acc 0 = Er.throwError 0
 f acc x | x `mod` 5 == 0 = do
                            s <- S.get 
                            S.put $! (s+1)
                            return $! max acc x
 f acc x = return $! max acc x

mainMax_MTL = print $ S.runState (Er.runErrorT (benchMax_MTL 1000000)) 0
-- bytecode
-- (Left 0,200000)
-- (3.84 secs, 1419124008 bytes)
{-
(Left 0,200000)
<<ghc: 296052344 bytes, 569 GCs, 28456/28456 avg/max bytes residency (1 samples), 1M in use, 0.00 INIT (0.00 elapsed), 0.08 MUT (0.08 elapsed), 0.00 GC (0.00 elapsed) :ghc>>
-}

-- Different order of layers
mainMax1_MTL = print $ 
   (S.runStateT (benchMax_MTL 1000000) 0 :: Either Int (Int,Int))
-- Left 0
-- (3.72 secs, 1389335288 bytes)
{-
Left 0
<<ghc: 278451768 bytes, 533 GCs, 28552/28552 avg/max bytes residency (1 samples), 1M in use, 0.00 INIT (0.00 elapsed), 0.07 MUT (0.07 elapsed), 0.00 GC (0.00 elapsed) :ghc>>
-}


benchMax_Eff :: (Member (Exc Int) r, Member (E.State Int) r) =>
                Int -> Eff r Int
benchMax_Eff n = foldM f 1 [n, n-1 .. 0]
 where
 f acc 0 = E.throwError (0::Int)
 f acc x | x `mod` 5 == 0 = do
                            s <- E.get 
                            E.put $! (s+1::Int)
                            return $! max acc x
 f acc x = return $! max acc x


mainMax_Eff = print $ 
 ((run $ E.runState (E.runError (benchMax_Eff 1000000)) 0) :: 
     (Either Int Int,Int))
-- bytecode
-- (Left 0,200000)
-- (3.87 secs, 1696071064 bytes)
{-
(Left 0,200000)
<<ghc: 625654800 bytes, 1124 GCs, 29120/29120 avg/max bytes residency (1 samples), 1M in use, 0.00 INIT (0.00 elapsed), 0.26 MUT (0.26 elapsed), 0.01 GC (0.01 elapsed) :ghc>>

with OpenUnion3.hs, lazy state
(Left 0,200000)
<<ghc: 663260008 bytes, 1183 GCs, 10498030/37323448 avg/max bytes residency (7 samples), 88M in use, 0.00 INIT (0.00 elapsed), 0.28 MUT (0.28 elapsed), 0.20 GC (0.22 elapsed) :ghc>>

-}

mainMax1_Eff = print $ 
 ((run $ E.runError (E.runState (benchMax_Eff 1000000) 0)) :: 
     Either Int (Int,Int))
-- bytecode
-- Left 0
-- (3.80 secs, 1600051128 bytes)
{-
Left 0
<<ghc: 539254344 bytes, 957 GCs, 29064/29064 avg/max bytes residency (1 samples), 1M in use, 0.00 INIT (0.00 elapsed), 0.22 MUT (0.23 elapsed), 0.00 GC (0.01 elapsed) :ghc>>
-}

-- No error layer
-- (1000000,200001)
-- (3.37 secs, 1520278144 bytes)
{-
(1000000,200001)
<<ghc: 310453944 bytes, 596 GCs, 28840/28840 avg/max bytes residency (1 samples), 1M in use, 0.00 INIT (0.00 elapsed), 0.08 MUT (0.08 elapsed), 0.00 GC (0.00 elapsed) :ghc>>
-}

-- ------------------------------------------------------------------------
-- Non-determinism benchmark: Pythagorian triples

-- First benchmark, with non-determinism only

-- Stream from k to n
iota k n = if k > n then mzero else return k `mplus` iota (k+1) n

pyth1 :: MonadPlus m => Int -> m (Int, Int, Int)
pyth1 upbound = do
  x <- iota 1 upbound
  y <- iota 1 upbound
  z <- iota 1 upbound
  if x*x + y*y == z*z then return (x,y,z) else mzero

pyth20 =
  [(3,4,5),(4,3,5),(5,12,13),(6,8,10),(8,6,10),(8,15,17),(9,12,15),(12,5,13),
   (12,9,15),(12,16,20),(15,8,17),(16,12,20)]


-- There is no instance of MonadPlus for ContT
-- we have to make our own

instance Monad m => MonadPlus (ContT [r] m) where
  mzero = ContT $ \k -> return []
  mplus (ContT m1) (ContT m2) = ContT $ \k ->
    liftM2 (++) (m1 k) (m2 k)

pythr_MTL = pyth20 == ((runCont (pyth1 20) (\x -> [x])) :: [(Int,Int,Int)])

pythr_EFF = pyth20 == ((run . makeChoiceA $ pyth1 20) :: [(Int,Int,Int)])

mainN_MTL = print . length $
            ((runCont (pyth1 300) (\x -> [x])) :: [(Int,Int,Int)])

mainN_EFF = print . length $
            ((run . makeChoiceA $ pyth1 300) :: [(Int,Int,Int)])

{-
Jul 2015:

mainN_MTL
418
<<ghc: 1525145072 bytes, 2919 GCs, 36500/44312 avg/max bytes residency (2 samples), 1M in use, 0.00 INIT (0.00 elapsed), 0.56 MUT (0.57 elapsed), 0.02 GC (0.01 elapsed) :ghc>>

mainN_EFF
418
<<ghc: 8465511760 bytes, 16181 GCs, 82124578/497872592 avg/max bytes residency (14 samples), 1163M in use, 0.00 INIT (0.00 elapsed), 5.29 MUT (5.24 elapsed), 3.09 GC (3.50 elapsed) :ghc>>

Eff is slower 14 times

Repeating with the better makeChoiceA
418
<<ghc: 7381819504 bytes, 14134 GCs, 55148/65984 avg/max bytes residency (2 samples), 1M in use, 0.00 INIT (0.00 elapsed), 4.34 MUT (4.36 elapsed), 0.12 GC (0.13 elapsed) :ghc>>
4.46user 0.02system 0:04.48elapsed 99%CPU (0avgtext+0avgdata 10912maxresident)k
0inputs+0outputs (0major+747minor)pagefaults 0swaps

Eff is now slower only 8 times
-}

-- Adding state: counting the number of choices

pyth2 :: Int -> ContT [r] (S.State Int) (Int, Int, Int)
pyth2 upbound = do
  x <- iota 1 upbound
  y <- iota 1 upbound
  z <- iota 1 upbound
  cnt <- S.get
  S.put $! (cnt + 1)
  if x*x + y*y == z*z then return (x,y,z) else mzero

pythrNS_MTL :: ([(Int,Int,Int)],Int)
pythrNS_MTL = S.runState (runContT (pyth2 20) (\x -> return [x])) 0

pyth2E :: (Member (E.State Int) r, Member NdetEff r) =>
          Int -> Eff r (Int, Int, Int)
pyth2E upbound = do
  x <- iota 1 upbound
  y <- iota 1 upbound
  z <- iota 1 upbound
  cnt <- E.get
  E.put $! (cnt + 1::Int)
  if x*x + y*y == z*z then return (x,y,z) else mzero


pyth2Er :: ([(Int,Int,Int)],Int)
pyth2Er = run . (`E.runState` 0) . makeChoiceA $ pyth2E 20

mainNS_MTL =
  let (l,cnt) = S.runState (runContT (pyth2 300) (\x -> return [x])) 0
  in print (length (l::[(Int,Int,Int)]), (cnt::Int))

mainNS_EFF =
  let (l,cnt) = run . (`E.runState` 0) . makeChoiceA $ pyth2E 300
  in print (length (l::[(Int,Int,Int)]), (cnt::Int))

{-
mainNS_MTL

(418,27000000)
<<ghc: 6505459048 bytes, 12526 GCs, 124065198/678365184 avg/max bytes residency (23 samples), 1666M in use, 0.00 INIT (0.00 elapsed), 3.18 MUT (3.10 elapsed), 8.16 GC (8.75 elapsed) :ghc>>

mainNS_EFF
(418,27000000)
<<ghc: 31371625496 bytes, 60104 GCs, 181306295/826823384 avg/max bytes residency (41 samples), 2235M in use, 0.00 INIT (0.00 elapsed), 12.89 MUT (12.81 elapsed), 23.76 GC (24.62 elapsed) :ghc>>

Eff is slower only 3 times!

(418,27000000)
<<ghc: 24203790280 bytes, 46883 GCs, 54768/65224 avg/max bytes residency (2 samples), 1M in use, 0.00 INIT (0.00 elapsed), 10.79 MUT (10.85 elapsed), 0.24 GC (0.25 elapsed) :ghc>>
11.02user 0.07system 0:11.10elapsed 99%CPU (0avgtext+0avgdata 10944maxresident)k
0inputs+0outputs (0major+749minor)pagefaults 0swaps

And now Eff is faster than MTL!
-}