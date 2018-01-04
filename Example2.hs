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

-- Electric Piano Synthesizer
iPiano = proc () -> do
  freq <- pitch2freq ^<< pitch -< ()
  sweep <- adsr <<< (arr (const (ADSR 1E-8 0.002 1E-8 1E-8)) &&& gate) -< ()
  w1 <- (* dB (-13)) ^<< square -< freq
  w2 <- (* dB (-20)) ^<< vco saw -< freq * (2 ** ((12.03 + sweep * 12) / 12))
  envA <- adsr <<< (arr (const (ADSR 0.01 0.3 0.3 0.2)) &&& gate) -< ()
  envF <- adsr <<< (arr (const (ADSR 0.01 0.1 0.1 0.2)) &&& gate) -< ()
  dry <- lp2 <<< arr fst &&& lp2 -< ((2000 + envF * 4000, 0.5), (w1 + w2) * envA)
  -- CPU-friendly reverb
  l <- fbDelay $ delayLine 0 0.084 >>> (\x -> ((2000, 0.5), x * 0.6)) ^>> lp2 -< dry
  r <- fbDelay $ delayLine 0 0.103 >>> (\x -> ((2000, 0.5), x * 0.6)) ^>> lp2 -< dry
  returnA -< mono (dry * 0.5) + Stereo l r * mono 0.5

-- Utility functions
playChord c = mapM_ $ \x -> diatonicTranspose x c
rep n x = sequence_ $ replicate n x

-- Reusable rhythms and chords
add9 = music "1/2 v1 5 ^1 2 {4/2 3}"
powA = scoped $ music "1/2 1 5 {^1} 5"
powB = scoped $ music "1/2 1 5 2/2 {^1}"
sixth = music "1/4 {v3} 1"

-- The song separated in parts
intro = scoped $ do
  inst iPiano; music "1/2"
  let melA = music "5 4 {^1} 4"
      melB = music "5 4 1 4"
      melAB = melA >> melB
      chords = playChord add9 [-2, -1, 0]
  reGate (const $ 1/1) $ do
    rep 4 melAB <:> chords
    (rep 2 melAB >> rep 2 melA) <:> chords
  arpeggio (1/16) $ music "3/1 (v1 5 ^1 ^1 4 5) 1/1 ."

verse = scoped $ do
  inst iPiano; music "1/2"
  let bassLine = scoped $ music "v" >> do
        music ". {1/1 3} . {v7} 4 {1/1 7}"
        playChord powA [-3, 0, -2, -1]
        music "1 5 {^1 2 3} 5 {1/1 ^1}"
        music "1/2 {v6} 3/2 3"
        playChord powB [-1, -3]
        music "1/2 {v1} 3/2 5"
        playChord powB [-2, -1] 
        modal Major $ playChord add9 [7]
      patternA = do
        music "1/4 1 3 5"
        mapM_ (arpeggio $ 1/24) $ (music "1/1 (2 4)" <:>) <$> musics "[] 7"
      melody = do
        music "5/4 5"; patternA
        mapM (rhythm "3/4 1/4") $ musics <$> ["4 [^2]", "1 b1"]
        music "1/1 1 5 1"; arpeggio (1/24) $ music "(3 5)"
        music "1/2 4 {1/6 4 5 4} 2 {v7} 7/2 1 1/2 v5"
        arpeggio (1/24) $ music "5/4 (1 3 5)"; patternA
        music "1/2 4 #7 ^1 2"
        playChord sixth [2, 1, 0, -1]
        rep 4 $ music "1 5 4 5"
        rep 3 $ music "1 5 #3 5"
        music "1/2 1 v5"
  bassLine <:> melody

-- Main music arrangement
mainMusic = do
  bpm 80
  key "G#3"
  mode Minor
  -- intro
  bpm 100
  verse

main = putWAVEFile "Example2.wav" $ toWav $ synth $ compileMusic mainMusic

