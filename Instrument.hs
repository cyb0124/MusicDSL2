-- This file defines the instrument procedure AST
-- and implements the compilation of it.

{-# LANGUAGE RankNTypes, GADTs #-}

module Instrument(
  InstCtx(..), Inst, InstProc(..),
  feedback, sampFreq, time, gate, pitch, compileInst, Box
) where
import Prelude hiding ((.))
import Control.Category
import Control.Arrow hiding (second)
import Control.Applicative
import Control.Monad.ST
import Control.Monad.State.Lazy
import Unsafe.Coerce
import Data.Array.ST
import Data.Array.MArray
import Data.Bifunctor
import Stereo

-- A context accessible by instrument procedures during each sample
data InstCtx = MkInstCtx {
    icFS :: Double,   -- Sampling frequency
    icTime :: Double, -- Current time (number of beats elapsed)
    icGate :: Bool,   -- Whether the key is pressed
    icPitch :: Int    -- Current note (number of semitones from A4)
  }

-- Instrument procedure AST
data Inst a b where
  InstId    :: Inst a a
  InstThen  :: Inst a b -> Inst b c -> Inst a c
  InstFun   :: (a -> b) -> Inst a b
  InstFirst :: Inst a b -> Inst (a, c) (b, c)
  InstCtx   :: (InstCtx -> a) -> Inst () a
  InstLoop  :: s -> Inst (a, s) (b, s) -> Inst a b
  InstLoop' :: Int -> Inst (a, s) (b, s) -> Inst a b

instance Category Inst where
  id = InstId
  (.) = flip InstThen

instance Arrow Inst where
  arr = InstFun
  first = InstFirst

-- Create a feedback loop; takes an initial state and an state update computation
feedback = InstLoop

-- Context queries
sampFreq = InstCtx icFS -- Get the sampling frequency
time = InstCtx icTime   -- Get the number of beats elapsed
gate = InstCtx icGate   -- Get whether the key is pressed
pitch = InstCtx icPitch -- Get the current note (number of semitones from A4)

-- Top type for states
data Box where
  Box :: a -> Box

-- Compiled instrument procedure
data InstProc = InstProc {
    ipInit :: [Box],
    ipProc :: forall s. STArray s Int Box -> InstCtx -> ST s Stereo
  }

-- Assign state indices and collect initial states
gatherStates :: Inst a b -> (Inst a b, [Box])
gatherStates p = second reverse $ runState (f p) [] where
  f :: Inst a b -> State [Box] (Inst a b)
  f p = case p of
    InstId -> return p
    InstThen x y -> InstThen <$> f x <*> f y
    InstFun _ -> return p
    InstFirst p -> InstFirst <$> f p
    InstCtx _ -> return p
    InstLoop s p -> do
      p' <- f p
      i <- length <$> get
      modify (Box s:)
      return $ InstLoop' i p'

-- Compile instrument procedure
compileInst :: Inst () Stereo -> InstProc
compileInst p =
  let
    (p', init) = gatherStates p
    f :: Inst a b -> a -> STArray s Int Box -> InstCtx -> ST s b
    f p x s ctx = case p of
      InstId -> return x
      InstThen p1 p2 -> do
        y <- f p1 x s ctx
        f p2 y s ctx
      InstFun f -> return $ f x
      InstFirst p1 -> do
        y <- f p1 (fst x) s ctx
        return (y, snd x)
      InstCtx f -> return $ f ctx
      InstLoop' i p1 -> do
        Box s1 <- readArray s i
        (y, s2) <- f p1 (x, unsafeCoerce s1) s ctx
        writeArray s i $ Box s2
        return y
  in
    InstProc init $ f p' ()
