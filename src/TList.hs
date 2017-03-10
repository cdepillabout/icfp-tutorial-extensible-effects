{-# LANGUAGE TypeOperators, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- No overlapping instances!

-- A simple type-level sum library
-- At present, it is a bit ungainly. It will look much better
-- in GHC 7.4, with data kinds

module TList where

data Void w -- no constructors
instance Functor Void  -- no method since there are no Void values

-- A sum data type, for `composing' effects
-- In GHC 7.4, we should make it a list
-- (:>) :: (* -> *) -> (* -> List) -> (* -> List)
infixr 1 :>
data (a :> b) w = H (a w) | T (b w)

instance (Functor s, Functor t) => Functor (s :> t) where
    fmap f (H x) = H (fmap f x)
    fmap f (T x) = T (fmap f x)


-- Type-level naturals and Booleans
data Z
data S n
data HTrue
data HFalse

-- TCode :: * -> Nat
type family TCode (n :: * -> *) :: *

-- Injection/projections

class Includes e s where
    inj :: e w -> s w
    prj :: s w -> Maybe (e w)

instance (b ~ TEQ (TCode e) (TCode e1), Includes' b e e1 t)
    => Includes e (e1 :> t)
 where
 inj = inj' (undefined::b)
 prj = prj' (undefined::b)

class Includes' b e e1 s where
    inj' :: b -> e w -> (e1 :> s) w
    prj' :: b -> (e1 :> s) w -> Maybe (e w)

instance (e ~ e1) => Includes' HTrue e e1 t where
    inj' _ e = H e
    prj' _ (H e) = Just e
    prj' _ (T _) = Nothing

instance Includes e t => Includes' HFalse e e1 t where
    inj' _ e = T (inj e)
    prj' _ (H e) = Nothing
    prj' _ (T e) = prj e


-- TEQ :: Nat -> Nat -> Bool
type family TEQ n1 n2 :: *
type instance TEQ Z Z           = HTrue
type instance TEQ (S n) Z       = HFalse
type instance TEQ Z (S n)       = HFalse
type instance TEQ (S n1) (S n2) = TEQ n1 n2
