{-# LANGUAGE ExistentialQuantification #-}

-- A taste for extensible effects and masking
-- A warm-up with a more complex example: non-determinism

module Nondet where

import Control.Monad.Cont

-- We start with the Cont r monad. It is certainly a monad, for
-- all r. We don't need to prove it; It has been proven already.

-- The non-determinism effect
-- choice lst non-deterministically chooses one value from the lst
-- choice [] thus corresponds to failure
data Choose w = forall a. Choose [a] (a -> w)

newtype Mu f = In{unmu:: f (Mu f)}

-- We carry the effect in types, including the absence of effect!
-- The result of the computation: a value or an effect (request)
data VE a s self = Val a | E (s self)

-- Now, the answer-type is recursive

choice :: [a] -> Cont (Mu (VE w Choose)) a
choice lst = Cont (\k -> In . E $ (Choose lst k))

-- MonadPlus-like operators are expressible via choice

mzero' :: Cont (Mu (VE w Choose)) a
mzero' = choice []

-- The (inferred) type of mplus' shows the effect
mplus' :: Cont (Mu (VE w Choose)) b
    -> Cont (Mu (VE w Choose)) b
    -> Cont (Mu (VE w Choose)) b
mplus' m1 m2 = choice [m1,m2] >>= id


-- The interpreter
makeChoice :: Cont (Mu (VE a Choose)) a -> Cont w [a]
makeChoice (Cont m) = loop (m (In . Val))
 where
 loop (In (Val x))            = return [x]
 loop (In (E (Choose [] _)))  = return []
 loop (In (E (Choose [x] k))) = loop (k x)
 loop (In (E (Choose lst k))) = fmap concat $ mapM (loop . k) lst
 

-- Now, run no longer has any `error'
-- (compare with run in TermAlgebra.hs)
run :: Cont a a -> a
run (Cont m) = m id

-- Examples

add :: Cont w Int -> Cont w Int -> Cont w Int
add = liftM2 (+)

-- The type is inferred
ex1 :: Cont w Int
ex1 = return 1 `add` return 2

ex1r = run ex1
-- 3

-- The type is inferred
ex2 :: Cont (Mu (VE w Choose)) Int
ex2 = return 1 `add` choice [1,2]

-- The following won't type: unhandled effect!
-- ex2r = run ex2
{-
    Couldn't match expected type `Mu (VE w0 Choose)'
                with actual type `Int'
    Expected type: Cont (Mu (VE w0 Choose)) (Mu (VE w0 Choose))
      Actual type: Cont (Mu (VE w0 Choose)) Int
-}

-- The inferred type shows that ex21 is now pure
-- ex21 :: Cont w [Int]
ex21 = makeChoice ex2 

ex21r = run ex21
-- [2,3]

-- Pythagorean triples
ex3 :: Cont (Mu (VE w Choose)) (Int, Int, Int)
ex3 = do
  x <- choice [1..10]
  y <- choice [1..10]
  z <- choice [1..10]
  if x*x + y*y == z*z then return (x,y,z) else mzero'

ex3r = run . makeChoice $ ex3
-- [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
