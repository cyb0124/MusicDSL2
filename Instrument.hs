-- This file defines the instrument procedure AST
-- and implements the compilation of it.

{-# LANGUAGE RankNTypes, GADTs, Strict, StrictData #-}

module Instrument(
  InstCtx(..), Inst, InstProc(..),
  feedback, feedbackR, time, gate, pitch, compileInst, Box
) where
import Prelude hiding ((.))
import Control.Category
import Control.Arrow
import Control.Applicative
import Control.Monad.ST
import Control.Monad.State.Lazy
import Unsafe.Coerce
import Data.Array.ST
import Data.Array.MArray
import qualified Data.Bifunctor as B
import Stereo

-- A context accessible by instrument procedures during each sample
data InstCtx = MkInstCtx {
    icTime :: Double, -- Current time (number of beats elapsed)
    icGate :: Bool,   -- Whether the key is pressed
    icPitch :: Int    -- Current note (number of semitones from A4)
  }

-- Instrument procedure AST
data Inst a b where
  InstId      :: Inst a a
  InstThen    :: Inst b c -> Inst a b -> Inst a c
  InstFun     :: (a -> b) -> Inst a b
  InstFirst   :: Inst a b -> Inst (a, c) (b, c)
  InstSecond  :: Inst a b -> Inst (c, a) (c, b)
  InstCombine :: Inst a b -> Inst c d -> Inst (a, c) (b, d)
  InstFanout  :: Inst a b -> Inst a c -> Inst a (b, c)
  InstCtx     :: (InstCtx -> a) -> Inst p a
  InstLoop    :: (Double -> s) -> Inst (a, s) (b, s) -> Inst a b
  InstLoop'   :: Int -> Inst (a, s) (b, s) -> Inst a b

-- Class instances

instance Show (Inst a b) where
  show x = case x of
    InstId -> "id"
    InstThen x y -> "then (" ++ show x ++ ") (" ++ show y ++ ")"
    InstFun _ -> "<function>"
    InstFirst x -> "first (" ++ show x ++ ")"
    InstSecond x -> "second (" ++ show x ++ ")"
    InstCombine x y -> "combine (" ++ show x ++ ") (" ++ show y ++ ")"
    InstFanout x y -> "fanout (" ++ show x ++ ") (" ++ show y ++ ")"
    InstCtx _ -> "<ctx>"
    InstLoop _ x -> "loop (" ++ show x ++ ")"

instance Category Inst where
  id = InstId
  (.) = InstThen

instance Arrow Inst where
  arr = InstFun
  first = InstFirst
  second = InstSecond
  (***) = InstCombine
  (&&&) = InstFanout

-- Create a feedback loop; takes an initial state and an state update computation
feedback s = InstLoop (const s)

-- Create a feedback loop with a function to randomize the initial state
feedbackR = InstLoop

-- Context queries
time = InstCtx icTime   -- Get the number of beats elapsed
gate = InstCtx icGate   -- Get whether the key is pressed
pitch = InstCtx icPitch -- Get the current note (number of semitones from A4)

-- Top type for states
data Box where
  Box :: a -> Box

-- Compiled instrument procedure
data InstProc = InstProc {
    ipInit :: [Double -> Box],
    ipProc :: forall s. STArray s Int Box -> InstCtx -> ST s Stereo
  }

-- Assign state indices and collect initial states
gatherStates :: Inst a b -> (Inst a b, [Double -> Box])
gatherStates p = B.second reverse $ runState (f p) [] where
  f :: Inst a b -> State [Double -> Box] (Inst a b)
  f p = case p of
    InstId -> return p
    InstThen x y -> InstThen <$> f x <*> f y
    InstFun _ -> return p
    InstFirst p -> InstFirst <$> f p
    InstSecond p -> InstSecond <$> f p
    InstCombine x y -> InstCombine <$> f x <*> f y
    InstFanout x y -> InstFanout <$> f x <*> f y
    InstCtx _ -> return p
    InstLoop s p -> do
      p' <- f p
      i <- length <$> get
      modify ((Box . s):)
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
        y <- f p2 x s ctx
        f p1 y s ctx
      InstFun f -> return $ f x
      InstFirst p1 -> do
        y <- f p1 (fst x) s ctx
        return (y, snd x)
      InstSecond p1 -> do
        y <- f p1 (snd x) s ctx
        return (fst x, y)
      InstCombine p1 p2 -> do
        y1 <- f p1 (fst x) s ctx
        y2 <- f p2 (snd x) s ctx
        return (y1, y2)
      InstFanout p1 p2 -> do
        y1 <- f p1 x s ctx
        y2 <- f p2 x s ctx
        return (y1, y2)
      InstCtx f -> return $ f ctx
      InstLoop' i p1 -> do
        Box s1 <- readArray s i
        (y, s2) <- f p1 (x, unsafeCoerce s1) s ctx
        writeArray s i $ Box s2
        return y
  in
    InstProc init $ f p' ()
