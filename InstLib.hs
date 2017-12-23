-- A library for making synthesizers

{-# LANGUAGE Arrows, Strict, StrictData #-}

module InstLib(pitch2freq, vco, saw, ADSR(..), adsr) where
import Prelude hiding ((.))
import Control.Category
import Control.Arrow
import Data.Fixed
import Instrument

pitch2freq x = 440 * (2 ** (fromIntegral x / 12))

phaseIntegrator :: Inst Double Double
phaseIntegrator = proc freq -> do
  feedback 0 $ proc (freq, s) -> do
    sr <- sampFreq -< ()
    returnA -< (s * 2 * pi, mod' (s + freq / sr) 1)
  -< freq

vco :: (Double -> Double) -> Inst Double Double
vco waveFn = arr waveFn . phaseIntegrator

saw :: Double -> Double
saw x =
  let x' = x / (2 * pi)
  in mod' (2 * x' + 1) 2 - 1

data ADSR = ADSR {atk :: Double, dcy :: Double, sus :: Double, rel :: Double}
data ADSRState = Atk | Dcy | Sus | Rel | AfterRel deriving (Eq, Ord)

adsr :: Inst (ADSR, Bool) Double
adsr = proc x -> do
  feedback (Atk, 0) $ proc ((param, gate), (state, level)) -> do
    sr <- sampFreq -< ()
    let atkRate = 1 / (sr * atk param)
        dcyRate = (1 - sus param) / (sr * dcy param)
        relRate = sus param / rel param
        nextLevel = case state of
          Atk -> level + atkRate
          Dcy -> level - dcyRate
          Sus -> level
          Rel -> level - relRate
          AfterRel -> level
        (nextLevel', nextState) = case state of
          Atk -> if nextLevel >= 1 then (1, Dcy) else (nextLevel, Atk)
          Dcy -> if nextLevel <= sus param then (sus param, Sus) else (nextLevel, Dcy)
          Sus -> (nextLevel, Sus)
          Rel -> if nextLevel <= 0 then (0, AfterRel) else (nextLevel, Rel)
          AfterRel -> (nextLevel, AfterRel)
        nextState' = if gate || nextState >= Rel then nextState else Rel
    returnA -< (level, (nextState', nextLevel'))
  -< x
