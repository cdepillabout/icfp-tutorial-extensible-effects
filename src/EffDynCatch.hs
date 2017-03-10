{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- Catching of dynamic exceptions
-- See the problem in
-- http://okmij.org/ftp/Haskell/misc.html#catch-MonadIO

module EffDynCatch where

import Eff1
import OpenUnion52

import qualified Control.Exception as Exc
import Data.Typeable

catchDynE :: forall e a r.
             (MemberU2 Lift (Lift IO) r, Exc.Exception e) =>
             Eff r a -> (e -> Eff r a) -> Eff r a
catchDynE m eh = interpose return h m
 where
   -- Polymorphic local binding: signature is needed
   h :: Lift IO v -> Arr r v a -> Eff r a
   h (Lift em) k = lift (Exc.try em) >>= \x -> case x of
         Right x -> k x
         Left  e -> eh e

modify :: Member (State s) r => (s -> s) -> Eff r ()
modify f = get >>= put . f


-- tests, from
-- http://okmij.org/ftp/Haskell/misc.html#catch-MonadIO

data MyException = MyException String deriving (Show, Typeable)
instance Exc.Exception MyException

exfn True = lift . Exc.throw $ (MyException "thrown")
exfn False = return True

testc m = catchDynE (m >>= return . show) (\ (MyException s) -> return s)


test1 = do runLift (tf True) >>= print; runLift (tf False) >>= print
 where
  tf x = runReader (runState (testc m) ([]::[String])) (x::Bool)
  m = do
      modify ("begin":)
      x <- ask
      r <- exfn x
      modify ("end":)
      return r
{-
("thrown",["begin"])
("True",["end","begin"])
-}


-- In CatchMonadIO, the result of tf True is ("thrown",[]) --
-- that is, an exception will drop the Writer's state, even if that
-- exception is caught. Here, the state is preserved!
-- So, this is an advantage over MTL!

-- Let us use an Error effect instead
test1' = do runLift (tf True) >>= print; runLift (tf False) >>= print
 where
  tf x = runReader (runState (runErrorStr (testc m)) ([]::[String])) (x::Bool)
  runErrorStr = asEStr . runError
  asEStr :: m (Either String a) -> m (Either String a)
  asEStr = id
  exfn True = throwError $ ("thrown")
  exfn False = return True
  m = do
      modify ("begin":)
      x <- ask
      r <- exfn x
      modify ("end":)
      return r

{-
(Left "thrown",["begin"])
(Right "True",["end","begin"])
-}
-- Now, the behavior of the dynamic Exception and Error effect is consistent.
-- The state is preserved. Before it wasn't.

test2 = do runLift (tf True) >>= print; runLift (tf False) >>= print
 where
  tf x = runReader (runState (runErrorStr (testc m)) ([]::[String])) (x::Bool)
  runErrorStr = asEStr . runError
  asEStr :: m (Either String a) -> m (Either String a)
  asEStr = id
  m = do
      modify ("begin":)
      x <- ask
      r <- exfn x `catchDynE` (\ (MyException s) -> throwError s)
      modify ("end":)
      return r

{-
(Left "thrown",["begin"])
(Right "True",["end","begin"])
-}

-- Full recovery
test2' = do runLift (tf True) >>= print; runLift (tf False) >>= print
 where
  tf x = runReader (runState (runErrorStr (testc m)) ([]::[String])) (x::Bool)
  runErrorStr = asEStr . runError
  asEStr :: m (Either String a) -> m (Either String a)
  asEStr = id
  m = do
      modify ("begin":)
      x <- ask
      r <- exfn x `catchDynE` (\ (MyException s) -> return False)
      modify ("end":)
      return r

{-
(Right "False",["end","begin"])
(Right "True",["end","begin"])
-}

-- Throwing within a handler

test3 = do runLift (tf True) >>= print; runLift (tf False) >>= print
 where
  tf x = runReader (runState (runErrorStr (testc m)) ([]::[String])) (x::Bool)
  runErrorStr = asEStr . runError
  asEStr :: m (Either String a) -> m (Either String a)
  asEStr = id
  m = do
      modify ("begin":)
      x <- ask
      r <- exfn x `catchDynE` (\ (MyException s) ->
                                lift . Exc.throw . MyException $
                                                     ("rethrow:" ++ s))
      modify ("end":)
      return r

{-
(Right "rethrow:thrown",["begin"])
(Right "True",["end","begin"])
-}

-- Implement the transnational behavior: when the exception is raised,
-- the state is rolled back to what it existed at the entrance to
-- the catch block.
-- This is the ``scoping behavior'' of `Handlers in action'

test_tran = do runLift (tf True) >>= print; runLift (tf False) >>= print
 where
  tf x = runReader (runState m1 ([]::[String])) (x::Bool)
  m1 = do
       modify ("init":)
       testc (transactionState (ProxyState :: ProxyState [String]) m)
  m = do
      modify ("begin":)
      x <- ask
      r <- exfn x
      modify ("end":)
      return r

{- -- without transaction
("thrown",["begin","init"])
("True",["end","begin","init"])
-}

-- With transaction
{-
("thrown",["init"])
("True",["end","begin","init"])
-}
