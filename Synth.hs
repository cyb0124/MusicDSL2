-- This module produces the final waveform from the compiled music and instruments

{-# LANGUAGE GADTs, Strict, StrictData #-}

module Synth(synth, toWav, sampFreq, normalize) where
import System.Random
import Control.Monad
import Control.Monad.ST
import Data.WAVE
import Data.Function
import Data.STRef
import Data.Array.MArray
import Data.Array.ST
import Debug.Trace
import Instrument
import Stereo
import Music

-- Constants
maxReleaseTime = 1 :: Double
sampFreq = 44100 :: Double

-- Instrument life
data InstLife where
  Triggered :: Double -> InstLife
  Released :: Double -> InstLife
  Died :: InstLife
  deriving Show

isTriggered (Triggered _) = True
isTriggered _ = False

isAlive Died = False
isAlive _ = True

decreaseLife :: InstLife -> Double -> InstLife
decreaseLife life dt = case life of
  Triggered x -> if x <= dt then Released maxReleaseTime else Triggered $ x - dt
  Released x -> if x <= 1 / sampFreq then Died else Released $ x - 1 / sampFreq
  Died -> Died

-- Instrument state
data InstState s = InstState {
    isProc :: InstCtx -> ST s Stereo,
    isPitch :: Int,
    isLife :: STRef s InstLife
  }

-- Main synthesize function
synth :: [TimedEvent] -> [Stereo]
synth es = reverse $ runST $ do
  time <- newSTRef 0
  events <- newSTRef es
  insts <- newSTRef []
  bpm <- newSTRef 128
  output <- newSTRef []
  rng <- newSTRef $ mkStdGen 1234

  let useRng = do
        r <- readSTRef rng
        let (y, r') = random r
        writeSTRef rng r'
        return y

      procEvent e = case e of
        EvBPMChange (BPMChange x) -> writeSTRef bpm x
        EvNote note -> do
          let ip = compileInst $ nInst note
              init = ipInit ip
          init' <- forM init $ \f -> f <$> useRng
          state <- newListArray (0, length init - 1) init'
          life <- newSTRef $ Triggered $ realToFrac $ nDuration note
          let is = InstState {
              isProc = ipProc ip state,
              isPitch = nPitch note,
              isLife = life
            }
          modifySTRef insts (is:)

      procEvents = do
        events' <- readSTRef events
        case events' of
          [] -> return ()
          (e:es) -> do
            t <- readSTRef time
            if t >= realToFrac (teTime e) then
              do
                writeSTRef events es
                procEvent $ teEvent e
                procEvents
            else return ()

      shouldStop = do
        noInsts <- null <$> readSTRef insts
        noEvents <- null <$> readSTRef events
        return $ noInsts && noEvents

      procInst time dt state = do
        nowLife <- readSTRef $ isLife state
        let ctx = MkInstCtx {
                icTime = time,
                icGate = isTriggered nowLife,
                icPitch = isPitch state
              }
            newLife = decreaseLife nowLife dt
        writeSTRef (isLife state) newLife
        isProc state ctx

      procInsts time dt = do
        nowInsts <- readSTRef insts
        result <- sum <$> (forM nowInsts $ procInst time dt)
        newInsts <- filterM (\x -> isAlive <$> readSTRef (isLife x)) nowInsts
        writeSTRef insts newInsts
        return result

  fix $ \(~loop) -> do
    procEvents
    bpmNow <- readSTRef bpm
    timeNow <- readSTRef time
    let dt = bpmNow / 60 / sampFreq
    nowOutput <- procInsts timeNow dt
    modifySTRef output (nowOutput:)
    writeSTRef time $ timeNow + dt
    continue <- not <$> shouldStop
    when continue loop

  readSTRef output

-- Convert sample list to wav format
toWav :: [Stereo] -> WAVE
toWav x = WAVE (WAVEHeader 2 (round sampFreq) 32 Nothing) $
  (\(Stereo l r) -> [doubleToSampleChecked l, doubleToSampleChecked r]) <$> x

doubleToSampleChecked x =
  if x > 1 then trace "clip" $ doubleToSample 1
  else if x < -1 then trace "clip" $ doubleToSample (-1)
  else doubleToSample x

-- Make maximum volume 0 dB
normalize :: [Stereo] -> [Stereo]
normalize xs = (* mono scale) <$> xs where
  maxL = maximum $ abs . lChn <$> xs
  maxR = maximum $ abs . rChn <$> xs
  scale = recip $ max maxL maxR
