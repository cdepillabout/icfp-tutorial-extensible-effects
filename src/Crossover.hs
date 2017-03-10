{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase, DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

-- Generic crossover: exchange suitable segments between two
-- data structures, generically

-- This library is for generic surgery on a data structure:
-- cutting it at an arbitrary place and grafting new branches.
-- The library can be used to implement many kinds of crossover.
-- The library shows off extensible effects, the ease
-- of defining a new effect and combining it with the two
-- standard effects, state and non-determinism.

module Crossover where

import Data.Data
import Data.Dynamic
import Control.Monad (liftM2)

-- Using extensible effects framework
-- I use Eff.hs in this directory
-- One can adjust the code to use any other ExtEff framework
import Eff
import OpenUnion2

-- First, we write an operation to traverse a data structure,
-- possibly replacing one branch.
-- The traversal stops when the tree is first updated

-- count of changes to the data structure
-- we exit the traversal when enough branches were updated
-- We need state so to exchange information between children traversals,
-- done by gfoldl.
newtype Updates = Updates Int deriving Typeable

-- The function is a wrapper over gfoldl, counting the updates
traverse :: Member (State Updates) r =>
      (forall a. (Data a) => a -> Eff r a) ->
      (forall a. (Data a) => a -> Eff r a)
traverse f = check_done $ \x -> f x >>= check_done traverse_children
 where
 threshold = 1
 check_done go x = get >>= \case
   Updates n | n >= threshold -> return x
   _                          -> go x
 traverse_children = gfoldl traverse_child return
 traverse_child builda x = liftM2 ($) builda (traverse f x)

-- Now, we differentiate traverse

-- When we resume with Nothing, we mean use the old value
data YieldD v = forall a. Data a => YieldD a (Maybe a -> v)
    deriving Typeable

instance Functor YieldD where
  fmap f (YieldD x k) = YieldD x (f . k)

yieldD :: (Data a, Member (State Updates) r, Member YieldD r) =>
          a -> Eff r a
yieldD x = send (inj . YieldD x) >>= \case
            Nothing -> return x
            Just x  -> modify (\ (Updates n) -> Updates (n+1)) >> return x

-- The data structure with a cut-off branch
data Cut r a = CDone a | Cut (YieldD (Eff r (Cut r a)))

-- Launch a thread and report its status
-- runCut :: Eff (YieldD :> r) a -> Eff r (Cut r a)
runCut m = loop (admin m) where
 loop (Val x) = return $ CDone x
 loop (E u)   =
   handle_relay u loop $  \(YieldD x k) -> return . Cut $ YieldD x (loop . k)


-- Differentiated traversal
traverse_diff :: Data a => a -> Eff r (Cut r (a,Updates))
traverse_diff x = runCut (runState (traverse yieldD x) (Updates 0))

-- Testing traverse_diff: traverse a tree and print out all branches
printDyn x = trace . go $ toDyn x
 where
   go x | Just y <- fromDynamic x = show (y::Bool)
   go x | Just y <- fromDynamic x = show (y::Int)
   go x | Just y <- fromDynamic x = show (y::[Int])
   go x = show x

traverse_all :: (Member Trace r, Data a) => a -> Eff r a
traverse_all x = loop =<< traverse_diff x
 where
   loop (CDone (x,_))      = return x
   loop (Cut (YieldD x k)) = printDyn x >> k Nothing >>= loop

tt1 = runTrace $ traverse_all (Just True)
{-
<<Maybe Bool>>
True
Just True
-}

tt2 = runTrace $ traverse_all [1::Int,2,3]
{-
[1,2,3]
1
[2,3]
2
[3]
3
[]
[1,2,3]
-}

zip_up :: Cut r a -> Eff r a
zip_up (CDone x)           = return x
zip_up (Cut (YieldD x k))  = zip_up =<< k (Just x)

-- Walk to a random branch in a data structure
random_walk :: (Member Choose r, Data a) => a -> Eff r (Cut r (a,Updates))
random_walk a = traverse_diff a >>= check
  where
    check y@CDone{}            = return y
    check y@(Cut (YieldD x k)) = return y `mplus'` (k Nothing >>= check)

-- cross-over: randomly walk to a branch in tree x and tree y
-- and swap the branches if their types are compatible
-- After swapping, return the updated trees.
-- We permit only one swapping.
crossover :: (Member Choose r, Data a, Data b) => a -> b -> Eff r (a,b)
crossover x y = do
  tx <- random_walk x  
  ty <- random_walk y
  -- Recall, Cut also includes the case when the whole data structure
  -- is cut, and what is left is the top hole
  case (tx,ty) of
    (Cut (YieldD x kx), Cut (YieldD y ky))
      | Just x' <- cast x, Just y' <- cast y -> do
      (xnew,_) <- zip_up =<< kx (Just y')
      (ynew,_) <- zip_up =<< ky (Just x')
      return (xnew,ynew)
    _ -> mzero'



-- test data structures
tdata1 = [1::Int, 2, 3]
tdata2 = [10::Int, 20]
tdata3 = [[100::Int], [200, 300]]

-- Evaluate the following to see the cross-over results

testc0 = run . makeChoice $ crossover (Just True) (Just False)
-- [(Just False,Just True),(Just False,Just True)]

testc01 = run . makeChoice $ crossover tdata1 (Just (10::Int)) 
-- [([10,2,3],Just 1),([1,10,3],Just 2),([1,2,10],Just 3)]

testc1 = run . makeChoice $ crossover tdata1 tdata2
{-
[([10,20],[1,2,3]),
 ([20],[10,1,2,3]),
 ([],[10,20,1,2,3]),
 ([10,2,3],[1,20]),
 ([20,2,3],[10,1]),
 ([1,10,20],[2,3]),
 ([1,20],[10,2,3]),
 ([1],[10,20,2,3]),
 ([1,10,3],[2,20]),
 ([1,20,3],[10,2]),
 ([1,2,10,20],[3]),
 ([1,2,20],[10,3]),
 ([1,2],[10,20,3]),
 ([1,2,10],[3,20]),
 ([1,2,20],[10,3]),
 ([1,2,3,10,20],[]),
 ([1,2,3,20],[10]),
 ([1,2,3],[10,20])]
-}

testc2 = run . makeChoice $ crossover [tdata1] tdata3

data Tree = Leaf Int | Node Tree Tree
                       deriving (Show, Data, Typeable)

tree1 = Node (Leaf 1) (Leaf 2)
tree2 = Node (Leaf 10) (Node (Leaf 20) (Leaf 30))

testc3 = run . makeChoice $ crossover tree1 tree2

