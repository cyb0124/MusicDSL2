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

drums = [
    kick >>^ (*0.5) >>^ mono,
    hihat >>^ (*0.1) >>^ pan (-0.2),
    snare >>^ (*0.3) >>^ pan (0.2),
    ride >>^ (*0.1) >>^ pan (0.3)
  ]

playDrums xs = sequence_ $ replicate 4 $
  foldr1 (<:>) $ zipWith (\i m -> inst i >> music m) drums xs

drumLoopA = [
    "1 . . 1 1 . . .",
    "1 1 1 1 1 1 1 1",
    ". . 1 . . . 1 .",
    ". . . . . . . 1"
  ]

drumLoopB = [
    "1 . . . 1 1 . .",
    "{1/4 . 1 1 1 1 . 1 1} 1 1 1 1",
    ". . 1 . . . 1 .",
    ". . . . . . . 1"
  ]

mainMusic = do
  bpm 128
  music "1/2"
  playDrums drumLoopA
  playDrums drumLoopB

main = putWAVEFile "Example1.wav" $ toWav $ synth $ compileMusic mainMusic
