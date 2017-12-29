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
  coupled <- hp1 -< (20, filtered)
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
fadeInLP2 :: (Double -> Double) -> Double -> Music a -> Music a
fadeInLP2 env q m = do
  t <- realToFrac <$> getTime
  reInst (\i -> (time >>^ (\freq -> (freq, q)) . env . subtract t) &&& i >>> stereoFilter lp2) m

-- Pad instrument
iPad = do
  delayTime <- (\x -> 60 / x / 2) <$> getBPM
  return $ proc () -> do
    wave <- unison (vco analogSaw) 7 <<< (\x -> (25, 1.5, 0.5, 0.5, pitch2freq x)) ^<< pitch -< ()
    envA <- adsr <<< (arr (const (ADSR 0.02 1.0 0.5 0.05)) &&& gate) -< ()
    let amped = wave * mono (dB (-7) * envA)
    fbDelay [delayLine (mono 0) delayTime >>^ (* mono 0.4)] -< amped

-- Pad melody
mPadCommon x = do
  inst =<< iPad; duration (4/1)
  let chord = music "(1 ^1 #3 5 ^1)"
  mapM_ (\x -> scoped (music x >> chord)) x

mPadIntro = fadeInLP2 (poly [(0, 100), (4, 8000), (8, 12000), (12, 20000)]) 1
  $ mPadCommon ["+P1", "+m3", "+P4",
      "1/4 +m6", "1/4 . . +m6", "1/4 . . +m6",
      "1/4 . +m7", "1/4 . . +m7", "1/4 . . +m7"]
mPadLoop = mPadCommon ["+P1", "+m3", "+P4", "2/1 +m6", "2/1 +m7"]

-- Main music arrangement
theMusic = do
  bpm 128
  key "Bb3"
  mode Minor
  mLead
  mLead <:> mPadIntro

main = putWAVEFile "Example3.wav" $ toWav $ synth $ compileMusic theMusic
