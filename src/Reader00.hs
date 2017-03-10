{-# LANGUAGE RankNTypes #-}

-- Extensible effects: the warm-up 
-- A single Reader Int effect as an interaction with an admin
-- This is the code for Sec 3.1 of the paper

module Reader00 where

import Control.Monad

-- A monadic library for communication between a handler and
-- its client, the administered computation

-- Status of a coroutine (client): done with the value of type w,
-- or requesting the current value of the Int environment
data VE w = Val w | E (Int -> VE w)

-- The Eff monad (not a transformer!)
-- It is actually
--     type Eff r = forall w. Cont (VE w r)
-- We inline it into Cont to put forall under newtype;
-- it is awkward otherwise in Haskell.
-- Also, in MTL, Cont is defined via transformers. We want to
-- avoid transformers!
newtype Eff a = Eff{runEff :: forall w. (a -> VE w) -> VE w}

-- standard instances for a continuation monad
instance Functor Eff where
    fmap f m = Eff $ \k -> runEff m (k . f)

instance Monad Eff where
    return x = Eff $ \k -> k x
    m >>= f  = Eff $ \k -> runEff m (\v -> runEff (f v) k)

-- send a request and wait for a reply
send :: (forall w. (a -> VE w) -> (Int -> VE w)) -> Eff a
send f = Eff (E . f)

-- administer a client: launch a coroutine and wait for it
-- to send a request or terminate with a value
admin :: Eff w -> VE w
admin (Eff m) = m Val

-- The signature is inferred
ask :: Eff Int
ask = Eff (\k -> E k)

-- The handler of Reader requests. The return type shows that
-- all requests are fully handled, nothing is left.
runReader :: Eff w -> Int -> w
runReader m e = loop (admin m) where
 loop :: VE w -> w
 loop (Val x)  = x
 loop (E k)    = loop (k e)


-- Locally rebind the value in the dynamic environment
-- This function is like a relay; it is both an admin for Reader requests,
-- and a requestor of them
local :: (Int -> Int) -> Eff w -> Eff w
local f m = do
  e0 <- ask
  let e = f e0
  let loop (Val x) = return x
      loop (E k)   = loop (k e)
  loop (admin m)

-- Examples
add :: Eff Int -> Eff Int -> Eff Int
add = liftM2 (+)

-- The type is inferred
t1 :: Eff Int
t1 = ask `add` return (1::Int)

-- The type is inferred
t1r :: Int
t1r = runReader t1 10
-- 11

t2 :: Eff Int
t2 = t1 `add` local (+100) t1

t2rr = runReader t2 10
-- 122
