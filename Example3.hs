{-# LANGUAGE Arrows #-}

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Monad
import Data.WAVE
import Music
import Synth
import Theory
import Stereo
import InstLib
import DrumLib
import Instrument
import SequenceParser

-- Lead instrument
iLead = proc () -> do
  wave <- pulse <<< (\x -> (0.3, pitch2freq x)) ^<< pitch -< ()
  envA <- adsr <<< (arr (const (ADSR 0.01 0.5 0.2 0.05)) &&& gate) -< ()
  envF <- adsr <<< (arr (const (ADSR 0.08 0.2 0.0 0.01)) &&& gate) -< ()
  filtered <- tanh ^<< lp2 -< ((envF * 1600 + 800, 1.8), wave)
  coupled <- hp1 -< (20, filtered) -- AC coupling
  returnA -< pan 0.2 $ envA * coupled * dB (-13)

-- Lead melody
mLead = do
  inst iLead; duration (3/2)
  scoped $ mapM_ music $ [
      "1 1 {1/2 1 1}",
      "3 3 {2/2 3}",
      "4 4 {1/2 4 4}", "v 3/4",
      "6 6 {2/4 6}",
      "7 7 {2/4 7}"
    ]

-- Make a fade-in effect using LP2
fadeInLP2 :: (Double -> Double) -> (Double -> Double) -> Double -> Music a -> Music a
fadeInLP2 envF envC q m = do
  t <- syncInst
  reInst (\i -> t >>> proc t -> do
      wave <- i -< ()
      filtered <- stereoFilter lp2 -< ((envF t, q), wave)
      let a = envC t
      returnA -< mono a * wave + mono (1 - a) * filtered
    ) m

-- Pad instrument
iPad = do
  delayTime <- (\x -> 60 / x / 2) <$> getBPM
  return $ proc () -> do
    wave <- unison (vco analogSaw) 7 <<< (\x -> (25, 1.5, 0.5, 0.5, pitch2freq x)) ^<< pitch -< ()
    envA <- adsr <<< (arr (const (ADSR 0.02 1.0 0.5 0.05)) &&& gate) -< ()
    let amped = wave * mono (dB (-7) * envA)
    fbDelay [delayLine (mono 0) delayTime >>^ (* mono 0.4)] -< amped

-- Pad melody
mPad = do
  inst =<< iPad; duration (4/1)
  let chord = music "(1 ^1 #3 5 ^1)"
  mapM_ (\x -> scoped (music x >> chord)) [
    "+P1", "+m3", "+P4",
    "1/4 +m6", "1/4 . . +m6", "1/4 . . +m6",
    "1/4 . +m7", "1/4 . . +m7", "1/4 . . +m7"]

mPadIntro = fadeInLP2 (poly [(0, 100), (4, 8000)])
  (poly [(0, 0), (12 - 0.01, 0), (12, 1)]) 1 mPad

-- Bass instrument
iBass' = proc freq -> do
  envC <- poly [(0, 0), (0.1, 0.3)] ^<< localTime -< ()
  wave <- (\((x1, x2), c) -> x1 * c + x2 * (1 - c))
    ^<< first (vco tri &&& vco saw) -< (freq, envC)
  envA <- adsr <<< (arr (const (ADSR 0.02 0.1 0.8 0.05)) &&& gate) -< ()
  filtered <- lp2 -< ((400, 1), wave)
  returnA -< pan (-0.3) $ filtered * envA * dB (-6.5)

iBass = pitch >>> pitch2freq ^>> iBass'

-- Bass melody

mBass = scoped $ reGate (\x -> x - (1/16)) $ do
  inst iBass; duration (1/2); music "v"
  let segment = music "{3/2 1} {1/4 v5 ^1} . 1 5 1"
  mapM_ (\x -> scoped (music x >> segment)) ["+P1", "+m3", "+P4"]
  music "3/4 v6 6 2/4 6 3/4 7 7 2/4 7"

mBassIntro = scoped $ reGate (\x -> x - (1/16)) $ do
  duration (-1/1); rest; duration (1/1); t <- syncInst
  target <- (/4) <$> pitch2freq <$> getKey
  inst $ t >>> poly [(0, 100), (1, target)] ^>> iBass'; music "1"
  inst iBass; music "4/1 vv"
  music "1 3 4 2/1 6 7"

-- Drum instruments
iHat = hihat >>^ pan 0.2 . (* dB (-20))
iRide = ride >>^ pan 0.3 . (* dB (-20))
iKick = kick >>^ mono . (* dB (-13))
iTom x = kick' x >>^ mono . (* dB (-13))
iSnare = snare >>^ mono . (* dB (-17))
iSnare2 = iSnare >>> (\x -> (4000, x)) ^>> stereoFilter lp1

-- Drum loop A
mDrumA = scoped $ do
  music "1/2"
  let hats = inst iHat >> music     "1 1 1      1   1   1    1 1"
      rides = inst iRide >> music   ". . 1      .   .   .    1 ."
      kicks = inst iKick >> music   "1 1 . {1/4 1 . 1 . 1 .} . 1"
      snares = inst iSnare >> music ". . 1 {1/4 . 1 . 1 . .} 1 ."
  hats <:> kicks <:> snares <:> rides

-- Drum loop A to B
mDrumAB = scoped $ do
  duration (-2/4); rest; music "1/4";
  inst $ iSnare >>^ (* mono (dB (-6)))
  music "1 1"

-- Drum loop B
mDrumB = scoped $ do
  music "1/4"
  let kicks = inst iKick >> music   "{3/4 1 1 {2/4 1}}"
      snares = inst iSnare2 >> music ". 1 1 . 1 1 . 1"
      hats = inst iHat >> music     "1 1 1 1 1 1 1 1"
      rides = inst iRide >> music   ". . 1 . . 1 . 1"
  kicks <:> snares <:> hats <:> rides

-- Drum loop combined
mDrum = sequence_ [mDrumA, mDrumA, mDrumA, mDrumAB, mDrumB, mDrumB]

-- Drum intro
mDrumIntroRaw = scoped $ do
  music "1/2"
  let kicks = inst iKick >> music "1 1 1 1 1 1 1 1"
      toms = scoped $ music ". . . . 1/4"
        >> mapM_ (\x -> inst (iTom x) >> music "1") [3, 2.5, 2, 1.5]
      snares = scoped $ inst iSnare >> music ". . . . . . 1/4 1 1 1 1"
      hats = scoped $ inst iHat >> music ". . . . 1/4 1 1 1 1 1 1 1 1"
  kicks <:> toms <:> snares <:> hats

mDrumIntro = do
  music "12/1 ."
  t <- syncInst
  let env = mono . poly [(0, 0.5), (1, 0.7), (2, 0.7), (4, 1)]
  reInst (\i -> t >>> ((const () ^>> i) &&& arr env) >>^ uncurry (*)) mDrumIntroRaw

-- Main music arrangement
theMusic = do
  bpm 128
  key "Bb3"
  mode Minor
  mLead
  mLead <:> mPadIntro <:> mBassIntro <:> mDrumIntro
  mLead <:> mPad <:> mBass <:> mDrum
  mLead <:> mPad <:> mBass <:> mDrum

main = putWAVEFile "Example3.wav" $ toWav $ synth $ compileMusic theMusic
