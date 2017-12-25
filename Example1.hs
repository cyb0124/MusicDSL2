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
  env <- adsr -< (ADSR 0.001 1 0.1 0.1, nowGate)
  nowTime <- time -< ()
  returnA -< pan (sin (2 * pi * nowTime) * 30 / 180 * pi) (wave * env)

testMusic = do
  key "D4"
  mode Major
  bpm 150
  inst testInst
  music "2/1 (1 4 6) (2 5 7) (3 #5 7) (3 6 ^1) (v6 ^1 4) (v7 ^2 5) (v3 5 ^1) (1 3 5 b7)"

main = putWAVEFile "Example1.wav" $ toWav $ synth $ compileMusic testMusic
