-- A library of common synthesizer modules

{-# LANGUAGE Strict, StrictData #-}

module InstLib(
  pitch2freq, vco, saw, ADSR(..), adsr, localTime,
  pulse, square, fm, noise, unison, poly, tri,
  Biquad(..), biquad, lp1, lp2, hp1, hp2, stereoFilter
) where
import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import System.Random
import Data.Fixed
import Instrument
import Music
import Stereo
import Unison
import IIR

-- 12-TET
pitch2freq x = 440 * (2 ** (fromIntegral x / 12))

-- Used for oscillators
phaseIntegrator :: Inst Double Double
phaseIntegrator = feedback 0 $ (sampFreq &&& id) >>^ iteration where
  iteration (sampFreq, (freq, s)) = (s * 2 * pi, mod' (s + freq / sampFreq) 1)

-- Current time inside the instrument
localTime :: Inst p Double
localTime = feedback 0 $ (sampFreq &&& id) >>^ iteration where
  iteration (sampFreq, (_, s)) = (s, s + 1 / sampFreq)

-- Make a simple VCO from a given wave function
vco :: (Double -> Double) -> Inst Double Double
vco waveFn = waveFn ^<< phaseIntegrator

-- Pulse wave with variable duty-cycle
pulse :: Inst (Double, Double) Double
pulse = second phaseIntegrator >>^ comparator where
  comparator (duty, x) = if x / (2 * pi) < duty then 1 else -1

-- Square wave
square :: Inst Double Double
square = (\freq -> (0.5, freq)) ^>> pulse

-- Digital (perfect) sawtooth
saw :: Double -> Double
saw x = mod' (x / pi + 1) 2 - 1

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
noise = feedback (mkStdGen 1234) $ arr iteration where
  iteration (_, g) = let (y, g') = random g in (y * 2 - 1, g')

-- ADSR envelope parameters
data ADSR = ADSR {atk :: Double, dcy :: Double, sus :: Double, rel :: Double}
data ADSRState = Atk | Dcy | Sus | Rel deriving (Eq, Ord)

-- linear ADSR envelope generator
adsr :: Inst (ADSR, Bool) Double
adsr = feedback (Atk, 0) $ (sampFreq &&& id) >>^ iteration where
  iteration (sampFreq, ((param, gate), (state, level))) =
    let
      atkRate = 1 / (sampFreq * atk param)
      dcyRate = (1 - sus param) / (sampFreq * dcy param)
      relRate = sus param / (sampFreq * rel param)
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

-- Polyline
poly :: [(Double, Double)] -> Double -> Double
poly [(x0,y0)] x = y0
poly ((x0,y0):ps@((x1,y1):_)) x
  | x > x1 = poly ps x
  | otherwise = let a = (x - x0) / (x1 - x0) in y0 * (1 - a) + y1 * a
