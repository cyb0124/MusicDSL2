-- A library of common synthesizer modules

{-# LANGUAGE Arrows, Strict, StrictData #-}

module InstLib(pitch2freq, vco, saw, ADSR(..), adsr, pulse, square) where
import Prelude hiding ((.))
import Control.Category
import Control.Arrow
import Data.Fixed
import Instrument

-- 12-TET
pitch2freq x = 440 * (2 ** (fromIntegral x / 12))

-- Used for oscillators
phaseIntegrator :: Inst Double Double
phaseIntegrator = proc freq -> do
  feedback 0 $ proc (freq, s) -> do
    sr <- sampFreq -< ()
    returnA -< (s * 2 * pi, mod' (s + freq / sr) 1)
  -< freq

-- Make a simple VCO from a given wave function
vco :: (Double -> Double) -> Inst Double Double
vco waveFn = arr waveFn . phaseIntegrator

-- Pulse wave with variable duty-cycle
pulse :: Inst (Double, Double) Double
pulse = proc (duty, freq) -> do
  x <- phaseIntegrator -< freq
  let x' = x / (2 * pi)
  returnA -< if x' < duty then 1 else -1

-- Square wave
square :: Inst Double Double
square = proc freq -> do
  pulse -< (0.5, freq)

-- Digital (perfect) sawtooth
saw :: Double -> Double
saw x =
  let x' = x / (2 * pi)
  in mod' (2 * x' + 1) 2 - 1

-- ADSR envelope parameters
data ADSR = ADSR {atk :: Double, dcy :: Double, sus :: Double, rel :: Double}
data ADSRState = Atk | Dcy | Sus | Rel deriving (Eq, Ord)

-- linear ADSR envelope generator
adsr :: Inst (ADSR, Bool) Double
adsr = proc x -> do
  feedback (Atk, 0) $ proc ((param, gate), (state, level)) -> do
    sr <- sampFreq -< ()
    let atkRate = 1 / (sr * atk param)
        dcyRate = (1 - sus param) / (sr * dcy param)
        relRate = sus param / (sr * rel param)
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
    returnA -< (level, (nextState', nextLevel'))
  -< x
