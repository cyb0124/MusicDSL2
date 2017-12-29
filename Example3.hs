{-# LANGUAGE Arrows #-}

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Data.WAVE
import Music
import Synth
import Theory
import Stereo
import InstLib
import DrumLib
import Instrument
import SequenceParser

delay = feedback (mono 0) $ proc (x, s) -> do
  let out = x + s * mono 0.2
  delayed <- delayLine (mono 0) 0.2 -< out
  returnA -< (out, delayed)

testInst = proc () -> do
  freq <- pitch >>^ pitch2freq -< ()
  wave <- unison (fm (vco sin) (vco sin) <<^ (\freq -> (2, 2, freq))) 3 -< (25, 1, 1, 0.6, freq)
  env <- adsr <<< (arr (const (ADSR 0.01 0.01 1 0.2)) &&& gate) -< ()
  out <- delay -< mono env * wave
  returnA -< out

testMusic = do
  bpm 128
  key "Bb4"
  mode Minor
  inst testInst
  reGate (const (1/16)) $ music "1/2 6 3 {1/4 {^1} 7} 6 7 4 ^2 {1/1 1}"

main = putWAVEFile "Example3.wav" $ toWav $ synth $ compileMusic testMusic
