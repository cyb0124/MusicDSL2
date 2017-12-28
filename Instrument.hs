-- This file defines the instrument procedure AST
-- and implements the compilation of it.

{-# LANGUAGE RankNTypes, GADTs, Strict, StrictData #-}

module Instrument(
  InstCtx(..), Inst, InstProc(..),
  feedback, sampFreq, time, gate, pitch, compileInst, Box
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
    icFS :: Double,   -- Sampling frequency
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
  InstCtx     :: (InstCtx -> a) -> Inst p a
  InstLoop    :: s -> Inst (a, s) (b, s) -> Inst a b
  InstLoop'   :: Int -> Inst (a, s) (b, s) -> Inst a b

-- Optimization
contract :: Inst a b -> Maybe (Inst a b)
contract x = case x of
  InstThen x (InstThen y z) -> Just $ InstThen (InstThen x y) z
  InstThen (InstCtx f) x -> Just $ InstCtx f
  InstThen (InstFun f1) (InstFun f2) -> Just $ InstFun (f1 . f2)
  InstThen (InstThen x (InstFun f1)) (InstFun f2) -> Just $ InstThen x (InstFun (f1 . f2))
  _ -> Nothing

step :: Inst a b -> Maybe (Inst a b)
step x = contract x <|> deeper x where
  deeper x = case x of
    InstId -> Nothing
    InstThen x y -> (InstThen x <$> step y) <|> ((`InstThen` y) <$> step x)
    InstFun f -> Nothing
    InstFirst x -> InstFirst <$> step x
    InstCtx f -> Nothing
    InstLoop s f -> InstLoop s <$> step f

optimize x = case step x of
  Just x -> optimize x
  Nothing -> x

-- Class instances

instance Show (Inst a b) where
  show x = case x of
    InstId -> "id"
    InstThen x y -> "then (" ++ show x ++ ") (" ++ show y ++ ")"
    InstFun _ -> "<function>"
    InstFirst x -> "first (" ++ show x ++ ")"
    InstCtx _ -> "<ctx>"
    InstLoop _ x -> "loop (" ++ show x ++ ")"

instance Category Inst where
  id = InstId
  (.) = InstThen

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
gatherStates p = B.second reverse $ runState (f p) [] where
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
    (p', init) = gatherStates $ optimize p
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
      InstCtx f -> return $ f ctx
      InstLoop' i p1 -> do
        Box s1 <- readArray s i
        (y, s2) <- f p1 (x, unsafeCoerce s1) s ctx
        writeArray s i $ Box s2
        return y
  in
    InstProc init $ f p' ()
