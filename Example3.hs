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
  wave <- pulse <<< (\x -> (0.4, pitch2freq x)) ^<< pitch -< ()
  envA <- adsr <<< (arr (const (ADSR 0.01 0.5 0.2 0.05)) &&& gate) -< ()
  envF <- adsr <<< (arr (const (ADSR 0.08 0.2 0.0 0.01)) &&& gate) -< ()
  filtered <- tanh ^<< lp2 -< ((envF * 1600 + 800, 1.8), wave)
  coupled <- hp1 -< (20, filtered)
  returnA -< pan 0.2 $ envA * coupled * dB (-3)

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

-- Pad instrument
iPad = proc () -> do
  wave <- unison (vco saw) 7 <<< (\x -> (20, 2, 0.8, 0.5, pitch2freq x)) ^<< pitch -< ()
  envA <- adsr <<< (arr (const (ADSR 0.02 1.0 0.5 0.05)) &&& gate) -< ()
  returnA -< wave * mono (dB (-10) * envA)

-- Pad melody
mPad = do
  inst iPad; duration (4/1)
  scoped $ music $ "(1 5 ^1 #3) (v7 ^3 5 7) (v#6 ^1 4 #6) 2/1 (v6 ^3 6 ^1) (v7 ^4 7 ^2)"

testMusic = do
  bpm 128
  key "Bb3"
  mode Minor
  mPad

main = putWAVEFile "Example3.wav" $ toWav $ synth $ compileMusic testMusic
