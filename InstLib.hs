-- A library of common synthesizer modules

{-# LANGUAGE Strict, StrictData #-}

module InstLib(
  pitch2freq, vco, saw, ADSR(..), adsr, localTime,
  pulse, square, fm, noise, unison, poly, tri, analogSaw,
  Biquad(..), biquad, lp1, lp2, hp1, hp2, stereoFilter,
  delayLine, fbDelay, syncInst, reverb, stereoReverb
) where
import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import System.Random
import Data.Sequence as S
import Data.Hashable
import Data.Fixed
import Instrument
import Music
import Stereo
import Unison
import Synth
import IIR

-- 12-TET
pitch2freq x = 440 * (2 ** (fromIntegral x / 12))

-- Used for oscillators. Initial phase is randomized.
phaseIntegrator :: Inst Double Double
phaseIntegrator = feedbackR (* (2 * pi)) $ arr iteration where
  iteration (freq, s) = (s * 2 * pi, mod' (s + freq / sampFreq) 1)

-- Current time inside the instrument
localTime :: Inst p Double
localTime = feedback 0 $ arr iteration where
  iteration (_, s) = (s, s + 1 / sampFreq)

-- Make a simple VCO from a given wave function
vco :: (Double -> Double) -> Inst Double Double
vco waveFn = waveFn ^<< phaseIntegrator

-- Pulse wave with variable duty-cycle (may have DC component; needs coupling)
pulse :: Inst (Double, Double) Double
pulse = second phaseIntegrator >>^ comparator where
  comparator (duty, x) = if x / (2 * pi) < duty then 1 else -1

-- Square wave
square :: Inst Double Double
square = (\freq -> (0.5, freq)) ^>> pulse

-- Digital (perfect) sawtooth
saw :: Double -> Double
saw x = mod' (x / pi + 1) 2 - 1

-- Analog (RC curve) sawtooth
analogSaw :: Double -> Double
analogSaw x = 0.15885773 - exp (-x)

-- Triangle wave
tri :: Double -> Double
tri = poly [(0, 0), (pi / 2, 1), (3 * pi / 2, -1), (2 * pi, 0)]

-- Construct a FM oscillator from two oscillators (ratio, index, freq)
fm :: Inst Double Double -> Inst Double Double -> Inst (Double, Double, Double) Double
fm carrier modulator =
  let
    mFreq (ratio, index, freq) = freq * ratio
    mPath = mFreq ^>> (modulator &&& id)
    cFreq ((mWave, mFreq), (ratio, index, freq)) = freq + mFreq * index * mWave
  in (mPath &&& id) >>> cFreq ^>> carrier

-- White noise generator
noise :: Inst p Double
noise = feedbackR (mkStdGen . hash) $ arr iteration where
  iteration (_, g) = let (y, g') = random g in (y * 2 - 1, g')

-- ADSR envelope parameters
data ADSR = ADSR {atk :: Double, dcy :: Double, sus :: Double, rel :: Double}
data ADSRState = Atk | Dcy | Sus | Rel deriving (Eq, Ord)

-- analog ADSR envelope generator
adsr :: Inst (ADSR, Bool) Double
adsr = feedback (Atk, 0) $ arr iteration where
  iteration ((param, gate), (state, level)) =
    let
      ~atkRate = (1.5 - level) / (sampFreq * atk param)
      ~dcyRate = (level - sus param / 1.5) / (sampFreq * dcy param)
      ~relRate = level * 1.5 / (sampFreq * rel param)
      nextLevel = case state of
        Atk -> level + atkRate
        Dcy -> level - dcyRate
        Sus -> level
        Rel -> level - relRate
      (nextLevel', nextState) = case state of
        Atk -> if nextLevel >= 1 then (1, Dcy) else (nextLevel, Atk)
        Dcy -> if nextLevel <= sus param then (sus param, Sus) else (nextLevel, Dcy)
        Sus -> (nextLevel, Sus)
        Rel -> if nextLevel <= 0 then (0, Rel) else (nextLevel, Rel)
      nextState' = if gate then nextState else Rel
    in (level, (nextState', nextLevel'))

-- Convert a mono filter to a stereo filter
stereoFilter :: Inst (a, Double) Double -> Inst (a, Stereo) Stereo
stereoFilter filter = split ^>> (filter *** filter) >>^ uncurry Stereo where
  split (a, Stereo l r) = ((a, l), (a, r))

-- Polyline linear interpolation. Can be used for wave-shapes and envelopes.
poly :: (Ord a, Fractional a) => [(a, a)] -> a -> a
poly [(x0,y0)] x = y0
poly ((x0,y0):ps@((x1,y1):_)) x
  | x > x1 = poly ps x
  | otherwise = let a = (x - x0) / (x1 - x0) in y0 * (1 - a) + y1 * a

-- Delay line
delayLine :: a -> Double -> Inst a a
delayLine init length =
  let
    samples = round $ length * sampFreq
    initState = S.replicate samples init
    iteration (x, (s:<|ss)) = (s, ss|>x)
  in feedback initState $ arr iteration

-- "fanout" a list of arrows
flattenA :: Arrow a => [a b c] -> a b [c]
flattenA [] = arr $ const []
flattenA (x:xs) = (x &&& flattenA xs) >>^ uncurry (:)

-- feedback comb filter
fbDelay :: Num a => Inst a a -> Inst a a
fbDelay delayLine = feedback 0 $ uncurry (+) ^>> (id &&& (delayLine >>^ negate))

-- Provides instrument procedure the relative musical time
syncInst :: Music (Inst p Double)
syncInst = do
  t <- realToFrac <$> getTime
  return $ time >>^ (subtract t)

-- Reverberation (freeverb algorithm)
revLPF damp = feedback 0 $ arr iteration where
  iteration (x, s) = (s, (1 - damp) * x + damp * s)
revFbLPF fbAmt damp time = fbDelay $ delayLine 0 time >>> revLPF damp >>^ (*fbAmt)
reverb fbAmt damp spread =
  let
    filters = revFbLPF fbAmt damp <$> (/sampFreq) <$> (+spread) <$>
      [1557, 1617, 1491, 1422, 1277, 1356, 1188, 1116]
  in flattenA filters >>^ sum

stereoReverb fbAmt damp =
  split ^>> reverb fbAmt damp 0 *** reverb fbAmt damp 23 >>^ uncurry Stereo where
    split (Stereo l r) = (l, r)
