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

-- CPU-friendly reverb
simpleReverb = proc dry -> do
  l <- fbDelay $ delayLine 0 0.084 >>> (\x -> ((2000, 0.5), x * 0.6)) ^>> lp2 -< dry
  r <- fbDelay $ delayLine 0 0.103 >>> (\x -> ((2000, 0.5), x * 0.6)) ^>> lp2 -< dry
  returnA -< mono (dry * 0.5) + Stereo l r * mono 0.5

-- Electric Piano Synthesizer
iPiano = proc () -> do
  freq <- pitch2freq ^<< pitch -< ()
  sweep <- adsr <<< (arr (const (ADSR 1E-8 0.002 1E-8 1E-8)) &&& gate) -< ()
  w1 <- (* dB (-13)) ^<< square -< freq
  w2 <- (* dB (-20)) ^<< vco saw -< freq * (2 ** ((12.03 + sweep * 12) / 12))
  envA <- adsr <<< (arr (const (ADSR 0.01 0.3 0.3 0.2)) &&& gate) -< ()
  envF <- adsr <<< (arr (const (ADSR 0.01 0.1 0.1 0.2)) &&& gate) -< ()
  simpleReverb <<< lp2 <<< arr fst &&& lp2 -< ((2000 + envF * 4000, 0.5), (w1 + w2) * envA)

-- Synth Strings
iStrings = proc () -> do
  envLFO <- (\x -> 1 - exp (-x * 3)) ^<< localTime -< ()
  lfo <- vco sin -< 2
  freq <- pitch2freq ^<< pitch -< ()
  w <- lChn ^<< unison (vco saw) 5 -< (25, 1, 1, 0, freq + lfo * envLFO * 3)
  envA <- adsr <<< (arr (const (ADSR 0.2 1.5 0.4 0.05)) &&& gate) -< ()
  envF <- adsr <<< (arr (const (ADSR 0.1 0.8 0.3 0.10)) &&& gate) -< ()
  filtered <- lp2 -< ((4000 + envF * 8000, 0.5), w)
  simpleReverb -< filtered * envA * dB (-6)

-- Reverse Cymbal
reverseCymbal = do
  inst $ proc () -> do
    envA <- adsr <<< (arr (const $ ADSR 3 1 1 0.13) &&& id) <<< gate -< ()
    w1 <- noise -< ()
    w2 <- fm (vco tri) (vco sin) -< (1.2, 4, 1500)
    let f0 = 0.8 * w1 + 0.2 * w2
    f1 <- lp2 -< ((6000, 0.5), f0)
    f2 <- hp2 -< ((400, 0.5), f1)
    stereoReverb 0.5 0.3 0.5 -< mono $ envA * f2 * dB (-45)
  music "4/1 1"

-- Drums
iKick = kick' 0.8 >>^ mono. (* dB (-6))
iSnare = snare >>> (* dB (-11)) ^>> simpleReverb
iHat = hihat >>^ pan 0.2 . (* dB (-20))
playDrums drums xs = foldr1 (<:>) $ zipWith (\i m -> inst i >> music m) drums xs

-- Utility functions
playChords cs xs = sequence_ $ zipWith diatonicTranspose xs $ cycle cs
playChord c = playChords [c]
rep n x = sequence_ $ replicate n x
susM = sustain . music
susMs = mapM_ sustain . musics
when' p f = if p then f else id

-- Some chords
add9 = music "1/2 v1 5 ^1 2 {4/2 3}"
powA = scoped $ susM "1/2 1 5 {^1} 5"
powB = scoped $ susM "1/2 1 5 2/2 {^1}"
sixth = music "1/4 {v3} 1"

-- Intro
intro = scoped $ do
  music "1/2"
  let melA = music "5 4 {^1} 4"
      melB = music "5 4 1 4"
      melAB = melA >> melB
      chords = playChord add9 [-2, -1, 0]
  reGate (const $ 1/1) $ do
    rep 4 melAB <:> chords
    (rep 2 melAB >> rep 2 melA) <:> chords
  arpeggio (1/16) $ music "3/1 (v1 5 ^1 ^1 4 5) 1/1 ."

-- Verse
arpLeadIn = arpeggio (1/24) $ music "5/4 (1 3 5)"
motifVM1 offset = do
  timeShift offset $ music "1/4 1 3 5"
  mapM_ (arpeggio $ 1/24) $ (music "1/1 (2 4)" <:>) <$> musics "[] 7"
motifVM2 = do
  mapM (rhythm "3/4 1/4") $ musics <$> ["4 [^2]", "1 b1"]
  music "1/1 1 5 1"; arpeggio (1/24) $ music "(3 5)"
  music "1/2 4 {1/6 4 5 4} 2 {v7} 7/2 1 1/2 v5"
motifVM3 = do
  music "1/2 4 #7 ^1 2"; playChord sixth [2, 1, 0, -1]
  rep 4 $ music "1 5 4 5"; rep 3 $ music "1 5 #3 5"
motifVB1 = susM "1/2 {v6} 3/2 3"
motifVB2 = do
  susM "1/2 {v7} 4 {1/1 7}"; playChord powA [-3, 0, -2, -1]
  susM "1 5 {^1 2 3} 5 {1/1 ^1}"
motifVB3 = modal Major $ playChord (sustain add9) [7]

verseA = scoped $ do
  music "1/2"
  let melody = do
        music "5/4 5"; motifVM1 0; motifVM2;
        arpLeadIn; motifVM1 0; motifVM3; music "1/2 1 v5"
      bassLine = scoped $ music "v" >> do
        music ". {3/2 3}"; motifVB2; motifVB1
        playChord powB [-1, -3]; susM "1/2 {v1} 3/2 5";
        playChord powB [-2, -1]; motifVB3
  bassLine <:> melody

verseB = scoped $ do
  music "1/2"
  let melody = do
        arpLeadIn; motifVM1 (-1/11); motifVM2;
        arpLeadIn; motifVM1 (-1/11); motifVM3 <:> music "1/2 2 2"; music "1/1 1"
      bassLine = scoped $ music "v" >> do
        motifVB1; motifVB2; playChord powB [-2, -1, -3, 0, -2, -1]; motifVB3
      drumEnd = music "1/2" >> playDrums [iSnare, iHat]
        ["1", unwords $ replicate 13 "1"]
  bassLine <:> melody <:> rep 8 drumLoop
  bassLine <:> melody <:> (rep 6 drumLoop >> drumEnd)

-- Chorus
drumIntro = scoped $ revert (4/1) >> reverseCymbal <:> do
  music "2/1 . 1/2"
  playDrums [iKick, iSnare, iHat] [". 1 1 1", ". . 1 .", ". 1 1 1"]

drumLoop = music "1/2" >> playDrums [iKick, iSnare, iHat]
  ["1 . . . . 1 . .", ". . 1 . . . 1 .", "1 1 1 1 1 1 1 1"]

chorus = scoped $ do
  music "1/4"
  let oblique c xs = scoped $ mapM_ (\x -> x >> music c) $ musics xs
      start = do
        oblique "{v5}" "3 2 3 4"; oblique "{v2}" "2 1 {v7}"; music "v5 7^"
        oblique "{v3}" "1 {v7} 1 3 {v7} {v6} {v5}"; music "v5 7"
      melody = music "^" >> do
        start; oblique "1" "6 5 4 6"; oblique "{v5}" "5 4 3 5"
        oblique "{v6}" "4 3 2 1"; music "{1/2 (b1 v2) 1 2 v5}"
        start; music "{1/2 6} ^6 5 4 3 {1/2 2} 5 4 5 6 5 4 3 2 1/2"
        oblique "5" "1 b1"; music "2/1 1"
      chordA = susM "{1 5 ^3 1}"
      chordB = susM "{1 5 ^1 3}"
      bassLine = scoped $ music "vv 1/2" >> do
        susMs "{1 ^5 ^2 1} {{v5} ^#6 1/1 7} {{v6} ^3 ^1 v6} {{v3} ^3 1/1 5}"
        susMs "{(v4 ^4) ^^{1/4 (1 6) 5 4 3} 2} {(v2 ^2) ^^1/4 (5 v5) 4 3 2 1 v7}"
        susMs "{3/4 (v6 ^6) 1/4 ^7 ^1 2 3 5} {(v5 ^5) ^^3 1/1 2}"
        playChords [chordA, chordB] [0, -3, -2, -5]
        playChord (sustain powB) [3, 0]
        music "(6 ^4) {^3} {3/4 (5 ^2) 1/4 ^1} (1 5 ^1) 5 1/1 ^1"
  bassLine <:> melody <:> rep 8 drumLoop

chorusStrings = scoped $ do
  let bassLineA = music "{v1 5 6 3}"
  music "2/1"; inst iStrings
  bassLineA <:> music "^5 2 1 1/1 3 v7"
  music "{v4 1 4/1 2}" <:> music "{^1} 1/1 5 ^3 2 b1 5 4"
  bassLineA <:> music "1/1 5 ^3 2 v7 2/1 ^1 v7"
  music "v4 1 {1/1 6 5} ^1" <:> music "(4 ^1) 5 {1/1 (6 ^3) (#7 ^2)} ^(1 v5)"

-- Main music arrangement
mainMusic = do
  bpm 80; key "G#3"; mode Minor; inst iPiano
  intro
  bpm 100
  verseA; drumIntro
  chorus
  verseB; drumIntro
  chorus <:> chorusStrings
  scoped $ revert (4/1) >> reverseCymbal

main = putWAVEFile "Example2.wav" $ toWav $ normalize $ synth $ compileMusic mainMusic
