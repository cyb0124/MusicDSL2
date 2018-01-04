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
  filtered <- (\x -> tanh (x / 2) * 2) ^<< lp2 -< ((envF * 1600 + 800, 1.8), wave)
  coupled <- hp1 -< (20, filtered) -- AC coupling
  stereoReverb 0.84 0.2 0.5 -< pan 0.2 $ envA * coupled * dB (-52)

-- Define rhythm patterns
rhythm' r x = rhythm r $ repeat $ music x
a = rhythm' "3/2 [] 1/2 []"
b = rhythm' "3/2 [] 2/2"
c = rhythm' "3/4 [] 2/4"

-- Lead melody
mLead = scoped $ do
  inst iLead
  a "1"; b "3"; a "4"; c "{v6}" ; c "{v7}"

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
    let amped = wave * mono (dB (-5.5) * envA)
    fbDelay $ delayLine (mono 0) delayTime >>^ (* mono 0.4) -< amped

-- Pad melody
mPad = do
  inst =<< iPad; music "4/1"
  let chord x = scoped $ music x >> music "(1 ^1 #3 5 ^1)"
      rhythm m = music "1/4" >> forM_ [1, 0, 0, 1, 0, 0, 1, 0] (\x ->
        if toEnum x then m else rest)
  mapM_ chord ["+P1", "+m3", "+P4"]
  mapM_ (rhythm . chord) ["+m6", "+m7"]

mPadIntro = fadeInLP2 (poly [(0, 100), (4, 4000), (8, 4000), (12, 8000)])
  (poly [(0, 0), (12 - 0.01, 0), (12, 1)]) 1 mPad

-- Bass instrument
iBass' = proc freq -> do
  envC <- poly [(0, 0), (0.1, 0.3)] ^<< localTime -< ()
  wave <- (\((x1, x2), c) -> x1 * c + x2 * (1 - c))
    ^<< first (vco tri &&& vco saw) -< (freq, envC)
  envA <- adsr <<< (arr (const (ADSR 0.02 0.1 0.8 0.05)) &&& gate) -< ()
  filtered <- lp1 -< (400, wave)
  stereoReverb 0.61 0.2 0.5 -< mono $ filtered * envA * dB (-45)

iBass = pitch >>> pitch2freq ^>> iBass'

-- Bass melody
mBass = scoped $ reGate (\x -> x - (1/16)) $ do
  inst iBass; music "1/2 vv"
  let segment = music "{3/2 1} {1/4 v5 ^1} . 1 5 1"
  mapM_ (\x -> scoped $ music x >> segment) ["+P1", "+m3", "+P4"]
  c "{v6}"; c "{v7}"

mBassIntro = scoped $ reGate (\x -> x - (1/16)) $ do
  duration (-1/1)
  music ". 1/1 vv"
  t <- syncInst
  inst $ (t &&& (pitch >>^ pitch2freq)) >>>
    (\(t, f) -> poly [(0, 100), (1, f)] t) ^>> iBass'
  music "1"
  inst iBass
  music "4/1 1 3 4 2/1 6 7"

-- Drum instruments
iHat = hihat >>^ pan 0.2 . (* dB (-20))
iRide = ride >>^ pan 0.3 . (* dB (-20))
iKick = kick >>^ mono . (* dB (-13))
iTom x = kick' x >>^ mono . (* dB (-13))
iSnare = snare >>^ mono . (* dB (-17))
iSnare2 = iSnare >>> (\x -> (4000, x)) ^>> stereoFilter lp1

playDrums drums xs = foldr1 (<:>) $ zipWith (\i m -> inst i >> music m) drums xs

-- Drum loop A
mDrumA = scoped $ music "1/2" >> playDrums [iHat, iRide, iKick, iSnare] [
    "1 1 1      1   1   1    1 1",
    ". . 1      .   .   .    1 .",
    "1 1 . {1/4 1 . 1 . 1 .} . 1",
    ". . 1 {1/4 . 1 . 1 . .} 1 ."
  ]

-- Drum loop A to B
mDrumAB = scoped $ do
  duration (-2/4); music ". 1/4"
  playDrums [iSnare >>^ (* mono (dB (-6)))] ["1 1"]

-- Drum loop B
mDrumB = scoped $ music "1/4" >> playDrums [iKick, iSnare2, iHat, iRide] [
    "1 . . 1 . . 1 .",
    ". 1 1 . 1 1 . 1",
    "1 1 1 1 1 1 1 1",
    ". . 1 . . 1 . 1"
  ]

-- Drum loop combined
mDrum = sequence_ [mDrumA, mDrumA, mDrumA, mDrumAB, mDrumB, mDrumB]

-- Drum intro
mDrumIntro' = scoped $ do
  music "1/2"
  playDrums [iKick, iSnare, iHat] [
      "1 1 1 1 1 1 1 1",
      ". . . . . . 1/4 1 1 1 1",
      ". . . . 1/4 1 1 1 1 1 1 1 1"
    ] <:> (music ". . . . 1/4" >> mapM_ (\x -> inst (iTom x) >> music "1")
      [3, 2.5, 2, 1.5])

mDrumIntro = do
  music "12/1 ."
  t <- syncInst
  let env = mono . poly [(0, 0.5), (2 - 0.01, 1.5), (2, 0.7), (4, 1)]
  reInst (\i -> t >>> ((const () ^>> i) &&& arr env) >>^ uncurry (*)) mDrumIntro'

-- Main music arrangement
mainMusic = do
  bpm 128
  key "Bb3"
  mode Minor
  mLead
  mLead <:> mPadIntro <:> mBassIntro <:> mDrumIntro
  mLead <:> mPad <:> mBass <:> mDrum
  mLead <:> mPad <:> mBass <:> mDrum

main = putWAVEFile "Example3.wav" $ toWav $ synth $ compileMusic mainMusic
