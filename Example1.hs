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
  envGain <- adsr -< (ADSR 0.001 1 0.1 0.2, nowGate)
  envFilter <- adsr -< (ADSR 0.1 1.5 1E-6 1E-6, nowGate)
  let cutoff = 8000 * envFilter + 100
  wave' <- stereoFilter lp2 -< ((cutoff, 8), wave)
  returnA -< wave' * mono (envGain * 0.1)

testMusic = do
  key "D4"
  mode Major
  bpm 150
  inst testInst
  music "2/1 (1 4 6)"
  -- music "2/1 (1 4 6) (2 5 7) (3 #5 7) (3 6 ^1) (v6 ^1 4) (v7 ^2 5) (v3 5 ^1) (1 3 5 b7)"

main = putWAVEFile "Example1.wav" $ toWav $ synth $ compileMusic testMusic
