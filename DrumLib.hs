-- Drum kits

{-# LANGUAGE Strict, StrictData #-}

module DrumLib(kick, hihat, ride, snare) where
import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Instrument
import InstLib
import Stereo
import Music

-- Kick drum
kick :: Inst p Double
kick =
  let
    freq = poly [(0, 1200), (0.01, 180), (0.05, 80), (0.15, 40)]
    sAmp = dB . poly [(0, 0), (1, -40), (1.1, -1/0)]
    nAmp = dB . poly [(0, -30), (0.06, -60), (0.07, -1/0)]
    distort x = tanh (x * 0.7) / 0.7
    mix (sWave, (nWave, t)) = distort sWave * sAmp t + nWave * nAmp t
  in localTime >>> ((freq ^>> vco sin) &&& noise &&& id) >>^ mix

-- Hi-hat
hihat :: Inst p Double
hihat =
  let
    amp = dB . poly [(0, -60), (0.01, 0), (0.18, -40), (1.4, -60), (1.5, -1/0)]
    filterInput x = ((5000, 1.2), x)
    mix (wave, t) = wave * amp t
  in localTime >>> ((noise >>> filterInput ^>> hp2) &&& id) >>^ mix

-- Ride cymbal
ride :: Inst p Double
ride =
  let
    amp = dB . poly [(0, 0), (0.7, -60), (0.8, -1/0)]
    filterInput x = ((8000, 1.2), x)
    osc = const 8000 ^>> vco tri
    mix (nWave, (sWave, t)) = (nWave * dB (-3) + sWave * dB (-6)) * amp t
  in localTime >>> ((noise >>> filterInput ^>> hp2) &&& osc &&& id) >>^ mix

-- Snare drum
snare :: Inst p Double
snare =
  let
    freq = poly [(0, 9000), (0.01, 800), (0.03, 200)]
    sAmp = dB . poly [(0, -60), (0.01, 0), (0.03, 0), (0.15, -40), (0.2, -1/0)]
    nAmp = dB . poly [(0, -60), (0.02, -8), (0.04, 0), (0.2, -30), (0.3, -60), (0.4, -1/0)]
    cutoff = poly [(0, 20000), (0.1, 10000), (0.2, 8000), (0.3, 400)]
    distort x = tanh x
    mix (sWave, (nWave, t)) = distort $ sWave * sAmp t + nWave * nAmp t
  in localTime >>> ((freq ^>> vco tri) &&& ((arr cutoff &&& noise) >>> lp1) &&& id) >>^ mix
