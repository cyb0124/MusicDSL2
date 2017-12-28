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

drumLoop =
  let kicks = do
        inst $ kick >>^ (*0.5) >>^ mono
        music "1 . . . 1 1 . ."
      hats = do
        inst $ hihat >>^ (*0.1) >>^ pan (-0.2)
        music "{1/4 . 1 1 1 1 . 1 1} 1 1 1 1"
      snares = do
        inst $ snare >>^ (*0.3) >>^ pan (0.2)
        music ". . 1 . . . 1 ."
      rides = do
        inst $ ride >>^ (*0.1) >>^ pan (0.3)
        music ". . . . . . . 1"
  in music "1/2" >> kicks <:> hats <:> snares <:> rides

testMusic = do
  bpm 128
  sequence_ $ replicate 4 drumLoop

main = putWAVEFile "Example2.wav" $ toWav $ synth $ compileMusic testMusic
