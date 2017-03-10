{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE OverlappingInstances #-}

-- Handle-based IO with the assured open/close protocol, see README
-- This file contains the Security kernel. See EffRegionTest.hs for tests.
-- This is the final solution: lightweight monadic regions with
-- only type-level enforcement of region discipline

module EffRegion
    (SIO,			-- constructors not exported
     RegionEff,
     SHandle,

     Proxy(..), rparent, rgparent, rggparent,

     runSIO,
     newRgn,
     newSHandle, newSHandle',
     shDup,
     IOMode(..),		-- re-exported from System.IO
     BufferMode(..),		-- re-exported from System.IO

     shGetLine,
     shPutStrLn,
     shPutStr,
     shIsEOF,
     shSetBuffering,

     -- ByteString
     -- shGetSome,
     -- shPut,

     shReport,

     sNewIORef,			-- IORef in SIO
     sReadIORef,
     sWriteIORef
     ) where

import Eff1
import OpenUnion51
import FTCQueue1

import GHC.TypeLits (Nat(..), type (+), type (-), type (<=), KnownNat)
import System.IO
import Control.Exception
import Data.IORef
import Data.List (delete)
import Prelude hiding (catch)

-- import qualified Data.ByteString as B		-- for byte-string operations

-- The IO monad with safe handles and regions (SIO) is implemented as 
-- with extensible effects

-- Each region handler maintains the state listing all open 
-- handles assigned to the region.

-- Since we do IO with our handles, we may be tempted to use the Lift IO
-- framework (IO lifted to Eff). However, that would 
-- give the user the ability to do any IO and so defeat the safety
-- guarantees. The purpose is to restrict permissible IO
-- operations to an assured set. So, we define our own IO effect, SIO.
-- Since exceptions play so important role in regions,
-- we transform all IO errors into exceptions, so they would be
-- handled as usual in the effect interpreters. SIO handler does such
-- a reflection.

              -- data constructor not exported
              -- Any exception is reflected (although perhaps that is
              -- not needed)
data SIO a where
  DoIO :: IO a -> SIO (Either SomeException a)

-- A convenient abbreviation for the SIO constraint
type SMonadIO r = (SafeForRegion r,
                   Member SIO r, Member (Exc SomeException) r)

-- Lifting any IO to Eff. This function is used only internally
-- and it is NOT exported
lIO :: SMonadIO r => IO a -> Eff r a
lIO m = send (DoIO m) >>= either throwError return


-- The handler of SIO requests. It is terminal
topSIO :: Eff '[SIO, Exc SomeException] w -> IO w
topSIO (Val x) = return x
topSIO (E u q) | Just (DoIO m) <- prj u = try m >>= topSIO . qApp q
topSIO (E u q) | Just (Exc e) <- prj u = throw (e::SomeException)
                  -- Nothing cannot occur

-- For convience, we compose topSIO with newRgn, to start computation with
-- the root region
runSIO :: (forall s. Eff
              '[RegionEff (L 0 s), SIO, Exc SomeException] w) -> IO w
runSIO x = topSIO (newRgn x)

-- This constraint determines the safe effects for regions
-- Only safe effects can close region boundaries.
-- Non-determinism is clearly an unsafe effect and is hardly
-- compatible with regions. Unsafe effects may still be used,
-- but they must be handled within a region.

class SafeForRegion (r :: [* -> *])
instance SafeForRegion '[]
instance SafeForRegion r => SafeForRegion (SIO ': r)
instance SafeForRegion r => SafeForRegion (Exc SomeException ': r)
instance SafeForRegion r => SafeForRegion (RegionEff s ': r)
instance SafeForRegion r => SafeForRegion (Reader a ': r)
instance SafeForRegion r => SafeForRegion (State a ': r)
-- more can be added
                                           
-- A region is identified by a label, which is a pair of a unique Nat
-- and a type eigenvariable (such as 's' in ST s).

-- Now it is labeled just with the region name
newtype SHandle s = SHandle Handle	-- data ctor not exported

-- Data constructors are not exported
data RegionEff s a where
  RENew :: FilePath -> IOMode -> RegionEff s (SHandle s)
  -- Used for duplicating regions
  REForget  :: SHandle s -> RegionEff s ()
  REAcquire :: SHandle s' -> RegionEff s (SHandle s)
  

-- RegName n lst finds the label of the n-th occurrence of RegionEff s
-- in lst, which must exist
type family Ancestor (n::Nat) (lst :: [* -> *]) :: * where
  Ancestor 0 (RegionEff s ': lst)     = s
  Ancestor n (RegionEff s ': lst)     = Ancestor (n-1) lst
  Ancestor n  (t ': lst)              = Ancestor n lst

-- A few predefined values to refer to the current and a couple of
-- ancestor regions by their index
data Proxy (n::Nat) = Proxy

rparent   = Proxy :: Proxy 1
rgparent  = Proxy :: Proxy 2
rggparent = Proxy :: Proxy 3
               
-- Create a new handle and assign it to the current region 
newSHandle :: (SMonadIO r, s ~ Ancestor 0 r, Member (RegionEff s) r) => 
	      FilePath -> IOMode -> Eff r (SHandle s)
newSHandle = newSHandle' (Proxy::Proxy 0)

-- Create a new handle and assign it to the current or ancestor region
-- (as specified by the first argument)
newSHandle' :: (SMonadIO r, s ~ Ancestor n r, Member (RegionEff s) r) => 
	      Proxy n -> FilePath -> IOMode -> Eff r (SHandle s)
newSHandle' _ fname fmode = send (RENew fname fmode)

-- Why just s alone won't suffice. This is related to the subtlety
-- in the type family or class instance selection (that is, evaluating
-- the Member (RegionEff s) r constraint.
-- In the typical case,
-- Member (RegionEff s, [RegionEff s1, RegionEff s, ...])
-- the Member constraint will not be resolved if 's' is just the quantified
-- type variable (although it should, since the variable is actually rigid).
-- But GHC 7.8 will not do that.

-- So, we need an additional identifier, which could manifestly distinguish
-- the regions.
-- We can write it more efficiently, by extracting the label from the
-- first encountered identifier.
type family Length (lst :: [* -> *]) :: Nat where
  Length '[] = 0
  Length (RegionEff x ': t) = 1 + (Length t)
  Length (h ': t) = Length t

data L (n::Nat) k

-- A region maintains the list of open handles, which it closes when
-- the region is finished, normally or by an exception.
-- We are watching for the exceptions Exc defined in Eff1. However, the user
-- may define their own effect that abandones the execution. We should
-- watch for them too: so, strictly speaking newRgn should have another
-- constraint on r, that all effects there are known to the effect
-- library and can be dealt with (like Exc) or are safe. That other constraint
-- is the type class that defines what the region should do with the other
-- effects that go past it.

-- Incidentally, IOErrors don't invalidate the Handles.
-- For example, if EOF is reported, we may try to reposition the `file'
-- and read again. That's why in Posix, EOF and file errors can be cleared.

newRgn :: forall r a. SMonadIO r =>
          (forall s. Eff (RegionEff (L (Length r) s) ': r) a) -> Eff r a
newRgn m = loop [] m
 where
   loop :: [Handle] -> Eff (RegionEff (L (Length r) s) ': r) a -> Eff r a
   loop fhs (Val x) = close_fhs fhs >> return x
   loop fhs (E u q)  = case decomp u of
     Right (RENew fname fmode) -> do
       fh <- lIO $ openFile fname fmode -- may raise exc
       k (fh:fhs) (SHandle fh)
     Right (REForget (SHandle fh)) -> do
       shReport . unwords $ ["forgetting handle", show fh,
                             "among",show fhs]
       k (delete fh fhs) ()
     Right (REAcquire (SHandle fh)) -> do
       shReport . unwords $ ["adding the handle", show fh,
                             "to",show fhs]
       k (if fh `elem` fhs then fhs else fh:fhs) (SHandle fh)
     Left  u -> case prj u of
       Just (Exc e) -> close_fhs fhs >> throwError (e::SomeException)
       Nothing      -> E u (tsingleton (k fhs))
    where k s = qComp q (loop s)

       -- Close all file handles of a region
   close_fhs []  = return ()
   close_fhs fhs = send (DoIO (mapM_ close fhs)) >> return ()
   close :: Handle -> IO ()
   close fh = do
    hPutStrLn stderr $ "Closing " ++ show fh
    catch (hClose fh) (\(e::SomeException) ->
                        hPutStrLn stderr ("Error on close: " ++ show e))

-- Safe-handle-based IO...

-- Frequent constraint, we give it a name
type ActiveRegion s r = (Member (RegionEff s) r, SMonadIO r)

-- Before:
-- shGetLine :: (MonadRaise m1 m2, SMonadIO m2) => SHandle m1 -> m2 String
-- The handle is assigned to the current region or its ancestor.
-- So, we have to verify that the label of the handle is the prefix
-- (perhaps improper) of the label of the monad (label of the region).

shGetLine :: ActiveRegion s r => SHandle s -> Eff r String
-- But the body of the function is just like before
shGetLine (SHandle h) = lIO (hGetLine h)

shPutStrLn :: ActiveRegion s r => SHandle s -> String -> Eff r ()
shPutStrLn (SHandle h) = lIO . hPutStrLn h

shPutStr :: ActiveRegion s r => SHandle s -> String -> Eff r ()
shPutStr (SHandle h) = lIO . hPutStr h

shIsEOF :: ActiveRegion s r => SHandle s -> Eff r Bool
shIsEOF (SHandle h) = lIO (hIsEOF h)

shSetBuffering :: ActiveRegion s r =>
		  SHandle s -> BufferMode -> Eff r ()
shSetBuffering (SHandle h) = lIO . hSetBuffering h

{-
shGetSome :: (MonadRaise m1 m2, SMonadIO m2) =>
	     SHandle m1 -> Int -> m2 B.ByteString
shGetSome (SHandle h) = lIO . B.hGetSome h

shPut :: (MonadRaise m1 m2, SMonadIO m2) => SHandle m1 -> B.ByteString -> m2 ()
shPut (SHandle h) = lIO . B.hPut h
-}


-- Duplicate a handle, returning a handle that can be used in the parent
-- region (and can be returned from the current region as the result).
-- This operation prolongs the life of a handle based on a
-- _dynamic_ condition. If we know the lifetime of a handle statically,
-- we can execute liftSIO (newSHandle ...) to place the handle in the
-- corresponding region. If we don't know the lifetime of a handle
-- statically, we place it in the inner region, and then extend its lifetime
-- by reassigning to the parent region based on the dynamic conditions.

shDup :: (ActiveRegion s r, ActiveRegion s' r,
         s' ~ Ancestor n r,
         s ~ (L n1 e1), s' ~ (L n2 e2), n2 <= n1) => 
	 Proxy n -> SHandle s -> Eff r (SHandle s')
shDup _ h =
  send (REForget h) >> send (REAcquire h)


-- Useful for debugging
shReport :: SMonadIO r => String -> Eff r ()
shReport = lIO . hPutStrLn stderr

-- make IORef available with SIO, so we may write tests that attempt
-- to leak handles and computations with handles via assignment
-- Here, the signatures are almost the same as those in the SafeHandle
-- library

sNewIORef :: SMonadIO r => a -> Eff r (IORef a)
sNewIORef = lIO . newIORef

sReadIORef :: SMonadIO r => IORef a -> Eff r a
sReadIORef = lIO . readIORef

sWriteIORef :: SMonadIO r => IORef a -> a -> Eff r ()
sWriteIORef r v = lIO $ writeIORef r v
