
-- A taste for extensible effects and masking
-- We re-do the exception monad from TermAlgebra.hs

module Exceptions where

import Control.Monad.Cont

-- We start with the Cont r monad. It is certainly a monad, for
-- all r. We don't need to prove it; It has been proven already.

-- Raising and handling exceptions
-- (Hinze 2000 had no exception-handling form)
-- We carry the effect in types, including the absence of effect!

raise :: e -> Cont (Either e w) a
raise e = Cont (\_ -> Left e)

snag :: Cont (Either e a) a -> (e -> Cont r a) -> Cont r a
snag (Cont m) handle = 
    case m Right of
     Left e  -> handle e
     Right x -> return x


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
ex2 :: Cont (Either Int w) Int
ex2 = return 1 `add` raise 2

-- The following won't type: unhandled exception!
-- ex2r = run ex2
{-
    Couldn't match expected type `Either Int w0' with actual type `Int'
    Expected type: Cont (Either Int w0) (Either Int w0)
      Actual type: Cont (Either Int w0) Int
-}

-- The inferred type shows that ex21 is now pure
-- ex21 :: Cont r Int
ex21 = snag ex2 (\e -> return e)

ex21r = run ex21
-- 2

-- Multiple exception types (not at the same time: it is coming though)

-- ex3 :: Cont (Either Bool w) Int
ex3 = snag ex2 (\e -> raise (e > 3))

-- unhandled exception
-- ex3r = run ex3
{-
    Couldn't match expected type `Either Bool w0'
                with actual type `Int'
    Expected type: Cont (Either Bool w0) (Either Bool w0)
      Actual type: Cont (Either Bool w0) Int
-}

ex3r1 = run $
	return 100 `add` (snag ex3 (\e -> return $ if e then 10 else 20))
-- 120

-- ex4 :: Bool -> Cont (Either Int w) Int
ex4 b = if b then return (1::Int) else raise (2::Int)

ex4r = run (snag (ex4 True) return `add` snag (ex4 False) return)
-- 3
