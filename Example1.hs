{-# LANGUAGE Arrows #-}

import Control.Arrow
import Data.WAVE
import Music
import Synth
import Stereo
import Theory
import InstLib
import Instrument
import SequenceParser

testInst = proc () -> do
  nowPitch <- pitch -< ()
  nowGate <- gate -< ()
  wave <- unison (vco saw) 7 -< (25, 1.5, 0.5, 0.5, pitch2freq nowPitch)
  envGain <- adsr -< (ADSR 0.001 1 0.5 0.2, nowGate)
  envFilter <- adsr -< (ADSR 0.2 1.5 0.5 1E-6, nowGate)
  let cutoff = 8000 * envFilter + 100
  wave' <- stereoFilter lp2 -< ((cutoff, 1), wave)
  returnA -< wave' * mono (envGain * 0.2)

testMusic = do
  key "D4"
  mode Dorian
  bpm 128
  inst testInst
  music "4/1 (#3 5 ^1) (3 5 7) (1 4 6) 2/1 (3 b6 ^1) (4 7 ^2)"

main = putWAVEFile "Example1.wav" $ toWav $ synth $ compileMusic testMusic
