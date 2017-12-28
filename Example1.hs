import Control.Arrow
import Data.WAVE
import Music
import Synth
import Stereo
import Theory
import InstLib
import DrumLib
import Instrument
import SequenceParser

drumLoop =
  let kicks = do
        inst $ kick >>^ (*0.5) >>^ mono
        music "1 . . 1 1 . . ."
      hats = do
        inst $ hihat >>^ (*0.1) >>^ pan (-0.2)
        music "1 1 1 1 1 1 1 1"
      snares = do
        inst $ snare >>^ (*0.3) >>^ pan (0.2)
        music ". . 1 . . . 1 ."
  in music "1/2" >> kicks <:> hats <:> snares

testMusic = do
  key "Bb4"
  mode Dorian
  bpm 128
  drumLoop

main = putWAVEFile "Example1.wav" $ toWav $ synth $ compileMusic testMusic
