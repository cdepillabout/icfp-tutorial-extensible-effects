{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}

-- For later examples
{-# LANGUAGE TypeOperators, DataKinds, FlexibleContexts #-}

-- ** Freer Monad and Extensible interpreters

module Tutorial2 where

import Control.Monad (ap)

import OpenUnion52

-- Review our general way to give meaning to effectful computations

data Comp req a where
  Val :: a   -> Comp req a
  E   :: req x -> (x -> Comp req a) -> Comp req a

-- Effect signature

data Get e x where
    Get :: Get e e

ask :: Comp (Get e) e
ask = E Get Val

runReader :: e -> Comp (Get e) a -> a
runReader e (Val x)   = x
runReader e (E Get k) = runReader e $ k e

bind :: Comp req a -> (a -> Comp req b) -> Comp req b
bind (Val x) f = f x
bind (E r k) f = E r (\x -> bind (k x) f)

-- More convenient notation
-- rlExp2 =
--   bind ask $ \x ->
--   bind ask $ \y ->
--   Val (x + y + 1)

-- renaming Val and bind, so to get the benefit of the do notation

instance Functor (Comp req) where
    fmap f (Val x) = Val (f x)
    fmap f (E req cont) = E req $ fmap f . cont

instance Applicative (Comp req) where
    pure = return
    (<*>) mf ma = do
        f <- mf
        a <- ma
        pure $ f a

instance Monad (Comp req) where
  return = Val
  (>>=)  = bind

-- Simpler composition modes


rlExp2 = do
  x <- ask
  y <- ask
  return (x + y + 1)

rlExp3 = do
  x <- rlExp2
  y <- ask
  return (x * y - 1)

_ = runReader 2 rlExp3 :: Int


-- ** Monad, or Freer Monad!
-- (a less optimal instance thereof)


-- Other interpreters
feedAll :: [e] -> Comp (Get e) a -> Maybe a
feedAll _ (Val a) = Just a
feedAll []   _    = Nothing
feedAll (h : t) (E Get k)   = feedAll t (k h)

_ = feedAll [2,3,4] rlExp3 :: Maybe Int


-- Another effect, Put

data Put o x where
  Put :: o -> Put o ()

send :: req x -> Comp req x
send req = E req return

tell :: o -> Comp (Put o) ()
tell x = send (Put x)

wrExp m = do
  tell "a"
  x <- m
  tell (show x)
  tell "end"


runWriter :: Comp (Put o) x -> ([o],x)
runWriter (Val x) = ([], x)
runWriter (E (Put written) cont) =
    let (writtenLater, x) = runWriter $ cont ()
    in (written : writtenLater, x)

_ = runWriter (wrExp (return 1))


-- QUIZ: What other Writer interpreters to write?


-- Several effects

{-
rwExp' = do
  tell "begin"
  x <- rlExp3
  tell "end"
  return x
-}



-- rxExp0' x = if x then rlExp3 else (do {tell "1"; return (0::Int)})

-- Internal choice!

data Sum r1 r2 x where
  L :: r1 x -> Sum r1 r2 x
  R :: r2 x -> Sum r1 r2 x

injL :: Comp r1 x -> Comp (Sum r1 r2) x
injL (Val x)  = Val x
injL (E r k)  = E (L r) (injL . k)

injR :: Comp r2 x -> Comp (Sum r1 r2) x
injR (Val x)  = Val x
injR (E r k)  = E (R r) (injR . k)

rxExp0 x = if x then injL rlExp3 else (injR (do {tell "1"; return (0::Int)}))

rwExp = do
  injR $ tell "begin"
  x <- injL $ rlExp3
  injR $ tell "end"
  return x

-- How to interpret? Need projections
runReaderL :: e -> Comp (Sum (Get e) r2) a -> Comp r2 a
runReaderL env (Val x) = return x
runReaderL env (E (L Get) k) = runReaderL env $ k env
runReaderL env (E (R r2) k) = E r2 $ runReaderL env . k

_ = runWriter $ runReaderL 2 rwExp

-- Interpreter composition



-- Doing injR and injL, etc. is impractical
-- Need a better idea.
-- Suggestions?

-- EDLS, p8, 1st full paragraph

-- -----------------------------------------------------------------------
-- Open Union interface
{-
type Union (r :: [* -> *]) a
class Member (t :: k) (r :: [k])

inj    :: Member t r => t v -> Union r v
prj    :: Member t r => Union r v -> Maybe (t v)

decomp :: Union (t:r) v -> Either (Union r v) (t v)

The type of inj/prj really shows the union as a (multi)set.

decomp imposes the ordering. Dissatisfaction. What we really need
is something like the local instances with the closure seamntics.
And Haskell almost has what we need! (Implicit parameters).
-}

injC :: Member req r => Comp req x -> Comp (Union r) x
injC (Val x) = Val x
injC (E r k)  = E (inj r) (injC . k)


runReaderC :: e -> Comp (Union (Get e ': r)) a -> Comp (Union r) a
runReaderC env (Val x) = Val x
runReaderC env (E req cont) = case decomp req of
    Right Get -> runReaderC env $ cont env
    Left r -> E r (runReaderC env . cont)


rwExpC = do
  injC $ tell "begin"
  x <- injC $ rlExp3
  injC $ tell "end"
  return (x::Int)

-- What is the inferred type?

xx = runReaderC (2::Int) rwExpC

-- What is the inferred type?

-- Handling two effects at the same time

runState :: e -> Comp (Union (Get e ': Put e ': r)) a -> Comp (Union r) a
runState env (Val x) = return x
runState env (E req cont) = case decomp req of
    Right Get -> runState env $ cont env
    Left req' ->  case decomp req' of
        Right (Put env') -> runState env' $ cont ()
        Left req'' -> E req'' $ runState env . cont


ts11 = do
  injC $ tell (10 ::Int)
  x <- injC ask
  return (x::Int)

-- Inferred type?

-- Putting injC inside ask/tell to ease the notation

x2 = (runState (0::Int) ts11)

run :: Comp (Union '[]) a -> a
run (Val x) = x

-- What now?


ts21 = do
  injC $ tell (10::Int)
  x <- injC ask
  injC $ tell (20::Int)
  y <- injC ask
  return ((x+y) :: Int)
  -- return (x+y)
