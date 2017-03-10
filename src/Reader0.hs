{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Extensible effects: the warm-up 
-- A single Reader effect as an interaction with an admin
-- This is the code for Sec 3.1 of the paper

module Reader0 where

import Control.Monad

-- A monadic library for communication between a handler and
-- its client, the administered computation

-- Status of a coroutine (client): done with the value of type w,
-- or sending a request of type r
data VE w r = Val w | E (r (VE w r))

-- The Eff monad (not a transformer!)
-- It is actually
--     type Eff r = forall w. Cont (VE w r)
-- We inline it into Cont to put forall under newtype;
-- it is awkward otherwise in Haskell.
-- Also, in MTL, Cont is defined via transformers. We want to
-- avoid transformers!
newtype Eff r a = Eff{runEff :: forall w. (a -> VE w r) -> VE w r}

-- standard instances for a continuation monad
instance Functor (Eff r) where
    fmap f m = Eff $ \k -> runEff m (k . f)

instance Monad (Eff r) where
    return x = Eff $ \k -> k x
    m >>= f  = Eff $ \k -> runEff m (\v -> runEff (f v) k)

-- send a request and wait for a reply
send :: (forall w. (a -> VE w r) -> r (VE w r)) -> Eff r a
send f = Eff (E . f)

-- administer a client: launch a coroutine and wait for it
-- to send a request or terminate with a value
admin :: Eff r w -> VE w r
admin (Eff m) = m Val

-- ------------------------------------------------------------------------
-- The initial case, no effects

data Void v -- no constructors

-- The type of run ensures that all effects must be handled:
-- only pure computations may be run.
run :: Eff Void w -> w
run m = case admin m of Val x -> x
-- the other case is unreachable since Void has no constructors
-- Therefore, run is a total function if admin m terminates.


-- ------------------------------------------------------------------------
-- The Reader monad

-- The request for a value of type e from the current environment
newtype Reader e v = Reader (e -> v)

-- The signature is inferred
ask :: Eff (Reader e) e
ask = send Reader

-- The handler of Reader requests. The return type shows that
-- all requests are fully handled, nothing is left.
runReader :: forall e w. Eff (Reader e) w -> e -> Eff Void w
runReader m e = loop (admin m) where
 loop :: VE w (Reader e) -> Eff Void w
 loop (Val x)        = return x
 loop (E (Reader k)) = loop (k e)


-- Locally rebind the value in the dynamic environment
-- This function is like a relay; it is both an admin for Reader requests,
-- and a requestor of them
local :: (e -> e) -> Eff (Reader e) w -> Eff (Reader e) w
local f m = do
  e0 <- ask
  let e = f e0
  let loop (Val x) = return x
      loop (E (Reader k)) = loop (k e)
  loop (admin m)

-- Examples
add :: Eff r Int -> Eff r Int -> Eff r Int
add = liftM2 (+)

-- The type is inferred
t1 :: Eff (Reader Int) Int
t1 = ask `add` return (1::Int)

{- t1 is effectful, according to its type: can't run
t1r0 = run t1
    Couldn't match expected type `Void' with actual type `Reader Int'
    Expected type: Eff Void w0
      Actual type: Eff (Reader Int) Int
-}

-- The type is inferred
t1r :: Eff Void Int
t1r = runReader t1 10

-- Since t1r is pure according to its type, we can run it
t1rr = run t1r
-- 11

t2 :: Eff (Reader Int) Int
t2 = t1 `add` local (+100) t1

t2rr = run $ runReader t2 10
-- 122
