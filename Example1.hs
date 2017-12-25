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
  env <- adsr -< (ADSR 0.001 1 0.1 0.1, nowGate)
  returnA -< wave * mono env

testMusic = do
  key "D4"
  mode Major
  bpm 150
  inst testInst
  music "2/1 (1 4 6) (2 5 7) (3 #5 7) (3 6 ^1) (v6 ^1 4) (v7 ^2 5) (v3 5 ^1) (1 3 5 b7)"

main = putWAVEFile "Example1.wav" $ toWav $ synth $ compileMusic testMusic
