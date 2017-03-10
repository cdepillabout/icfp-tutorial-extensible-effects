{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- Extensible effects and effect masking

module Extensible where

import Control.Monad.Cont
import TList

abort :: e -> Cont e a
abort e = Cont (\_ -> e)

shift :: ((a -> w) -> Cont w w) -> Cont w a
shift f = Cont (runC . f) where runC (Cont m) = m id

-- The result of a computation: a value or an effect 
-- (that is, a request for service)
data VE a s w = Val a | E (s w)

-- Now, the answer-type is generally recursive
newtype Mu f = In{unmu:: f (Mu f)}

-- Now, run no longer has any `error'
-- (compare with run in TermAlgebra.hs)
-- The type of run ensures that all effects must be handled:
-- only pure computations may be run.
run :: Cont (Mu (VE a Void)) a -> a
run (Cont m) = case m (In . Val) of In (Val x) -> x
-- the other case is unreachable since Void has no constructors
-- Therefore, run is a total function if m Val terminates.

-- ------------------------------------------------------------------------
-- Add one effect: exceptions

newtype Exc e w = Exc e
instance Functor (Exc e) where
    fmap f (Exc e) = Exc e

type instance TCode (Exc e) = Z

-- The effect-raising form
class Raise e s where
    raise :: e (Mu (VE r s)) -> Cont (Mu (VE r s)) a

-- Interpreters of effects impose the order on effects
-- (and mask the handled effect)
-- The effects other than exceptions propagate up
-- The signature is a bit simplistic: the handler may have effects
-- other than those in the body. We need to join the effects of
-- the body and of the handler
snag :: Functor s =>
	Cont (Mu (VE a (Exc e :> s))) a -> (e -> Cont (Mu (VE w s)) a) ->
	Cont (Mu (VE w s)) a
snag (Cont m) handler = loop (m (In . Val))
 where 
 loop (In (Val x))         = return x
 loop (In (E (H (Exc e)))) = handler e
 loop (In (E (T x)))       = -- effect propagation
  shift (\k -> return . In . E $ fmap (\w -> runCont (loop w) k) x)


-- The effect raising forms should work with any order
instance Includes e s => Raise e s where
    raise e = abort (In (E (inj e)))


-- Examples

add :: Cont w Int -> Cont w Int -> Cont w Int
add = liftM2 (+)

-- The type is inferred
ex1 :: Cont w Int
ex1 = return 1 `add` return 2

ex1r = run ex1
-- 3

-- The type is inferred
-- ex2 :: Includes (Exc Int) s => Cont (Mu (VE r s)) Int
ex2 = return 1 `add` raise (Exc (2::Int))

-- The following won't type: unhandled exception!
-- ex2r = run ex2
{-
    No instance for (Includes (Exc Int) Void)
      arising from a use of `ex2'
-}

-- The inferred type shows that ex21 is now pure
-- ex21 :: Cont (Mu (VE w Void)) Int
ex21 = snag ex2 (\e -> return e)

ex21r = run ex21
-- 2

-- ------------------------------------------------------------------------
-- Add the second effect: non-determinism (choice)

-- choice lst non-deterministically chooses one value from the lst
-- choice [] thus corresponds to failure
data Choose w = forall a. Choose [a] (a -> w)

instance Functor Choose where
    fmap f (Choose lst k) = Choose lst (f . k)

type instance TCode Choose = S Z

class Choice s where
    choice :: [a] -> Cont (Mu (VE r s)) a

instance Includes Choose s => Choice s where
    choice lst = shift (\k -> return . In . E . inj $ Choose lst k)

-- MonadPlus-like operators are expressible via choice

mzero' = choice []
mplus' m1 m2 = choice [m1,m2] >>= id


-- The interpreter
makeChoice :: Functor s =>
	      Cont (Mu (VE a (Choose :> s))) a -> 
	      Cont (Mu (VE r s)) [a]
makeChoice (Cont m) = loop (m (In . Val))
 where
 loop (In (Val x))                = return [x]
 loop (In (E (H (Choose [] _))))  = return []
 loop (In (E (H (Choose [x] k)))) = loop (k x)
 loop (In (E (H (Choose lst k)))) = fmap concat $ mapM (loop . k) lst
 loop (In (E (T x)))       = -- effect propagation
  shift (\k -> return . In . E $ fmap (\w -> runCont (loop w) k) x)
 

-- The type is inferred
exc1 :: Includes Choose s => Cont (Mu (VE r s)) Int
exc1 = return 1 `add` choice [1,2]

-- Unhandled effect
-- exc1r = run exc1
{-
    No instance for (Includes Choose Void)
      arising from a use of `exc1'
-}

-- Inferred type shows that exc11 is pure
-- exc11 :: Functor s => Cont (Mu (VE r s)) [Int]
exc11 = makeChoice exc1

exc11r = run exc11
-- [2,3]


-- ------------------------------------------------------------------------
-- Combining exceptions and non-determinism

-- Exceptions and choice, together
-- The type is inferred. The type shows two effects --
-- but not their order!
exc2  :: (Includes Choose s, Includes (Exc Int) s) =>
     Cont (Mu (VE r s)) Int

exc2 = return (1::Int) `mplus'` (return 2 `mplus'` 
       (raise (Exc (3::Int)) `mplus'` return 4))


-- The order of effects is enforced by interpreters:
-- which interpreter goes first

-- case 1: exceptions over non-determinism

-- The inferred type shows that non-determinism is
-- handled, but exceptions are still left
exc2c :: (Includes (Exc Int) s, Functor s) => Cont (Mu (VE r s)) [Int]
exc2c = makeChoice exc2

-- The inferred type shows exc2ce is pure
-- exc2ce :: Functor s => Cont (Mu (VE w s)) [Int]
exc2ce = snag (makeChoice exc2) (\e -> return [e])

-- So, it can be run
-- The result shows that an exception abandoned all accumulated choices
exc2cer = run exc2ce
-- [3]

-- case 2: non-determinism over exceptions

-- The inferred type shows that exceptions are dealt with,
-- but non-determinism is left
exc2e :: (Includes Choose s, Functor s) => Cont (Mu (VE w s)) Int

exc2e = snag exc2 return

exc2ecr = run . makeChoice $ exc2e
-- [1,2,3,4]

-- Case 3: cut: interaction of two effects, handling of both
-- effects together. 
-- For the explanation of cut, see Section 5 of Hinze ICFP 2000 paper.
-- Hinze suggests expressing cut in terms of cutfalse
--  ! = return () `mplus` cutfalse
-- where
--  cutfalse :: m a
-- satisfies the following laws
--   cutfalse >>= k       = cutfalse
--   cutfalse `mplus` m   = cutfalse
-- (note: m `mplus` cutfalse is different from cutfalse `mplus` m)
--
-- Hinze also introduces the operation call :: m a -> m a that
-- delimits the effect of cut: call m executes m. If the cut is 
-- invoked in m, it discards only the choices made since m was called.
--
-- Hinze noted a problem with the `mechanical' derivation of backtracing
-- monad transformer with cut: no axiom specifying the interaction of 
-- call with bind; no way to simplify nested invocations of call.

-- We use exceptions for cutfalse
-- Therefore, the law ``cutfalse >>= k       = cutfalse''
-- is satisfied automatically since all exceptions have the above property.
cutfalse = raise (Exc ())

-- The interpreter -- it is like reify . reflect with a twist
-- Compare this implementation with the huge implementation of call
-- in Hinze 2000 (Figure 9)
call :: Functor s =>
	      Cont (Mu (VE a (Choose :> Exc e :> s))) a -> 
	      Cont (Mu (VE r (Choose :> s))) a
call (Cont m) = loop [] (m (In . Val))
 where
 loop jq (In (Val x))                = return x `mplus'` next jq
 loop jq (In (E (H (Choose [] _))))  = next jq
 loop jq (In (E (H (Choose [x] k)))) = loop jq (k x)
 loop jq (In (E (H (Choose lst k)))) = next $ map k lst ++ jq
 loop jq (In (E (T (H (Exc _)))))    = mzero'  -- drop jq
 loop jq (In (E (T (T x))))       = -- effect propagation
  shift (\k -> return . In . E . T $ fmap (\w -> runCont (loop jq w) k) x)

 next []    = mzero'
 next (h:t) = loop t h


exc2cut = run . makeChoice $ call exc2
-- [1,2]

exc3 = (return (1::Int) `mplus'` return 2) `mplus'` 
       ((cutfalse `mplus'` return 4) `mplus'`
        return 5)

exc3cut = run . makeChoice $ call exc3
-- [1,2]

exc4 = return (1::Int) `mplus'` 
       call (return 2 `mplus'` (cutfalse `mplus'` return 3) `mplus'` return 4) 
       `mplus'` return 5

-- Here we see nested call. It poses no problems...
exc4cut = run . makeChoice $ call exc4
-- [1,2,5]

-- ------------------------------------------------------------------------
-- Add another effect: environment (or Reader, or dynamic binding)

-- The request is to obtain the current value of the dynamically bound
-- cell of the type a
newtype Env e w = Env (e -> w)

instance Functor (Env e) where
    fmap f (Env k) = Env (f . k)

type instance TCode (Env e) = S (S Z)

class Reader e s where
    ask :: Cont (Mu (VE r s)) e

instance Includes (Env e) s => Reader e s where
    ask = shift (\k -> return . In . E . inj $ Env k)

-- The interpreter
runReader :: Functor s =>
	      e ->
	      Cont (Mu (VE a (Env e :> s))) a -> 
	      Cont (Mu (VE r s)) a
runReader v (Cont m) = loop (m (In . Val))
 where
 loop (In (Val x))          = return x
 loop (In (E (H (Env k))))  = loop (k v)
 loop (In (E (T x)))        = -- effect propagation
  shift (\k -> return . In . E $ fmap (\w -> runCont (loop w) k) x)

-- The inferred type tells all the effects, but not their order
exr1  :: (Includes Choose s, Includes (Env Int) s) =>
	 Cont (Mu (VE r s)) Int
exr1 = return (1::Int) `mplus'` ask

exr1run = run . runReader 10 $ makeChoice exr1
-- [1,10]


-- Rebinding of the dynamically-bound variable: cf. local in the
-- Reader monad

local :: (Functor s, Includes (Env e) s) =>
	      (e -> e) ->
	      Cont (Mu (VE a s)) a -> 
	      Cont (Mu (VE r s)) a
local modf (Cont m) = ask >>= \e -> loop (modf e) (m (In . Val))
 where
 loop v (In (Val x))          = return x
 loop v (In (E x)) | Just (Env k) <- prj x = loop v (k v)
 loop v (In (E x)) = -- effect propagation
  shift (\k -> return . In . E $ fmap (\w -> runCont (loop v w) k) x)

exr1run2 = run . runReader 10 $ makeChoice (local (+1) exr1)
-- [1,11]

exr1run3 = run . runReader 10 $ local (+1) (makeChoice exr1)
-- [1,11]


exr2 = exr1 `mplus'` 
       local (+ 2) (exr1 `mplus'` local (+ 3) exr1 `mplus'` exr1)

-- We see that the first local's modification is visible all throughout.
-- Yet the modification by (local (+3)) is visible only within its branch

exr2r = run . runReader 10 . local (+ 20) $ makeChoice exr2
-- [1,30,1,32,1,35,1,32]

-- The following snippet (from the code accompanying our ICFP06 paper
-- on delimited dynamic binding explains the problem with the transformers

{-
A more elaborate example of inexpressiveness of 
static layering of continuation and environment monad transformers

Let us consider the following, frequently occurring pattern.  This
pattern occurs in (and is adapted from) several examples mentioned in
the paper: in equations (5) and (6), and more practically in test4 in
new-parameters.scm.  These examples are based on discussions while we
wrote the paper: http://lambda-the-ultimate.org/node/1396#comment-16007
The Zipper file-system project shows more examples of the same pattern.

    (my-parameterize ((p 500))
      (let ((thread1 
             (push-prompt q
               (let ((v1 (p)))
                 (my-parameterize ((p 600))
                   (yield q)
                   (list v1 (p)))))))
        ...))

Whereas the continuation of the first use of the parameter p includes
the prompt q, the continuation captured by (yield q) includes the
second my-parameterize.  Thus, no static layering of environment and
continuation monad transformers can implement this pattern. The
examples below show that the translation of this code into
Haskell does not type-check, no matter how the two monad transformers
are layered.
-}


{-
Further plans:
	State and local state; two ways of combining state with non-determinism
	Control effects?

Unlike monad Zippers (ICFP11): No transformers.
Better traversing the monad stack?
Masking of the effects.

cutfalse: non-trivial interaction: handling of two effects together.

Unlike Swierstra: no overlapping instances. Mainly: masking of the effects,
local interpreters. We can not only add effects but also subtract them.
-}
