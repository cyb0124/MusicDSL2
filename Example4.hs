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

-- Lead instrument
iLead = proc () -> do
  envLFO <- (\x -> 1 - exp (-x * 3)) ^<< localTime -< ()
  lfo <- vco sin -< 6
  wave <- vco analogSaw <<< (\(x, y) -> pitch2freq x + y) ^<< (pitch &&& id)
    -< lfo * envLFO * 5
  envA <- adsr <<< (arr (const (ADSR 0.01 0.5 0.2 0.05)) &&& gate) -< ()
  envF <- adsr <<< (arr (const (ADSR 0.01 0.2 1E-6 1E-6)) &&& gate) -< ()
  filtered <- lp1 -< (4000 + envF * 12000, wave)
  stereoReverb 0.84 0.2 0.5 -< mono $ filtered * envA * dB (-38)

-- Lead music
mLead = scoped $ do
  inst iLead
  -- Reuse the same rhythm for different melodies
  let rhythm x melody = sequence_ $ zipWith (>>) (musics x) melody
      rhythmA = rhythm "1/1 1/2 1/1 1/2 [] []"
      rhythmB = rhythm "1/1 1/1 2/1"
      rhythmC = rhythm "2/1 1/1 1/1"
      rhythmD = rhythm "3/2 3/2 2/2"
  -- Automatically accompany each note with a fixed note
      parSingle r n m = r (musics m) <:> r (repeat $ scoped $ music n)
  -- Automatic parallel thirds
      par3 x = x <:> diatonicTranspose (-2) x
  rhythmA $ musics "[v6] 6 6 6 5 6"
  rhythmA $ musics "7 [^1] 2 1 [v7] 4"
  music "8/1 5"
  rhythmA $ musics "6 6 6 6 5 6"
  rhythmA $ musics "7 [^1] 2 4 5 4"
  music "4/1 5 3/1 ^1 1/1 v7"
  par3 $ rhythmA $ musics "[^1] 1 1 1 2 1"
  par3 $ rhythmB $ musics "2 3 4"
  parSingle rhythmA "v#7" "5 6 5 4 3 2"
  parSingle rhythmC "v5" "3 2 1"
  parSingle rhythmD "(v3 6)" "1 2 3"
  parSingle rhythmD "(v4 7)" "2 {v7} 2"
  music "4/1 #3 ^1" <:> music "8/1 (1 v5)"

-- Bass instrument
iBass = proc () -> do
  envC <- poly [(0, 0), (0.1, 0.3)] ^<< localTime -< ()
  wave <- (\((x1, x2), c) -> x1 * c + x2 * (1 - c))
    ^<< ((vco tri &&& vco saw <<< pitch2freq ^<< pitch) &&& id) -< envC
  envA <- adsr <<< (arr (const (ADSR 0.02 0.1 0.8 0.05)) &&& gate) -< ()
  filtered <- lp1 -< (400, wave)
  stereoReverb 0.84 0.2 0.5 -< mono $ filtered * envA * dB (-43)

-- Bass music
mBass = scoped $ do
  inst iBass; music "vvv"
  let loopA = music "1/1 1 1/2 5 1/1 1 1/2 1 3 5"
      playLoopA = mapM_ (\x -> diatonicTranspose x loopA)
  playLoopA [5, 6, 7, 7, 5, 6, 7, 7, 5]
  music "{1/1 7 ^2 2/1 4}"
  modal HarmMin $ playLoopA [4]
  music "{2/1 {^1} 1/1 7 5}"
  playLoopA [5, 6]
  modal Major $ playLoopA [7, 7]

-- Main music
theMusic = do
  key "A4"
  mode Minor
  bpm 128
  mLead <:> mBass

main = putWAVEFile "Example4.wav" $ toWav $ synth $ compileMusic theMusic
