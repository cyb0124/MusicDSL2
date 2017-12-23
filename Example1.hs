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
  wave <- vco saw -< pitch2freq nowPitch
  env <- adsr -< (ADSR 0.001 0.05 0.5 0.5, nowGate)
  returnA -< mono (wave * env)

testMusic = do
  key "D4"
  mode Major
  bpm 150
  inst testInst
  music "1/2 (1 3) 2 3 4 5"

main = putWAVEFile "Example1.wav" $ toWav $ synth $ compileMusic testMusic
