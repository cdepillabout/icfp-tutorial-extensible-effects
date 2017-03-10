{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
-- Only for MemberU below, when emulating Monad Transformers
{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}

-- Open unions (type-indexed co-products) for extensible effects
-- This implementation relies on _closed_ overlapping instances
-- (or closed type function overlapping soon to be added to GHC)

module OpenUnion3 (Union, inj, prj, decomp, 
                   Member, MemberU, MemberU2, (:>)
                  ) where

import Data.Typeable

-- parameter r is phantom: it just tells what could be in the union
-- This encoding is quite like that in the HList paper.
-- The data constructor Union is not exported

newtype Union r v =        -- r is of a kind [*->*] and phantom
  Union (forall t. (Functor t, Typeable1 t) => Maybe (t v))

newtype Id x = Id x                     -- for the sake of gcast1

instance Functor (Union r) where
    fmap f (Union p) = Union (maybe Nothing (Just . fmap f) p)

inj :: (Functor t, Typeable1 t, Member t r) => t v -> Union r v
inj x = Union (maybe Nothing (\(Id x) -> Just x) $ gcast1 (Id x))

prj :: (Functor t, Typeable1 t, Member t r) => Union r v -> Maybe (t v)
prj (Union p) = p

{-# INLINE decomp #-}
decomp :: (Functor t, Typeable1 t) => 
          Union (t :> r) v -> Either (Union r v) (t v)
decomp (Union p) | Just x <- p = Right x
decomp (Union p) = Left (Union p)

class Member (t :: * -> *) r
instance Member t (t :> r)
instance Member t r => Member t (t' :> r)

-- A sum data type, for `composing' effects
-- In GHC 7.4, we should make it a list
-- (:>) :: (* -> *) -> (* -> List) -> List
infixr 1 :>
data ((a :: * -> *) :> b)

-- This class is used for emulating monad transformers
class Member t r => MemberU (tag :: * -> * -> *) (t :: * -> *) r | tag r -> t
instance MemberU tag (tag e) (tag e :> r)
instance MemberU tag t r => MemberU tag t (t' :> r)

-- A version of MemberU for argument of a different kind.
-- Latest GHC has well-functioning PolyKind extension; therefore,
-- MemberU2 can be merged with MemberU.
class Member t r => 
      MemberU2 (tag :: (* -> *) -> * -> *) (t :: * -> *) r | tag r -> t
instance MemberU2 tag (tag e) (tag e :> r)
instance MemberU2 tag t r => MemberU2 tag t (t' :> r)
