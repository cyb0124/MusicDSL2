-- Implements unison detuning

{-# LANGUAGE Arrows, Strict, StrictData #-}

module Unison (unison) where
import Prelude hiding ((.))
import Control.Category
import Control.Arrow
import Instrument
import Stereo

detuneFreqs :: Int -> [Double]
detuneFreqs 0 = []
detuneFreqs n
  | odd n = 0 : detuneFreqs (n - 1)
  | otherwise =
      let n' = fromIntegral n
          scale = (n' - 2) / n'
      in -1 : 1 : ((*scale) <$> detuneFreqs (n - 2))

detuneFreqsDenser :: Int -> Double -> [Double]
detuneFreqsDenser n density = f <$> detuneFreqs n where
  f :: Double -> Double
  f x = signum x * (abs x ** density)

dupA :: Arrow a => Int -> a b c -> a [b] [c]
dupA 0 _ = arr $ const []
dupA n f = let fs = dupA (n - 1) f in proc (x:xs) -> do
  y <- f -< x
  ys <- fs -< xs
  returnA -< (y:ys)

unison :: Inst Double Double -> Int -> Inst (Double, Double, Double, Double, Double) Stereo
unison osc n =
  let
    rawTunes = detuneFreqs n
    oscs = dupA n osc
    predN = pred n
  in proc (detune, density, fade, wide, freq) -> do
    -- detune: [0, 25]
    -- density: [1, 4]
    -- fade: [0.5, 4]
    -- wide: [0, pi/4]
    let tunes = (\x -> signum x * (abs x ** density)) <$> rawTunes
        cents = (*detune) <$> tunes
        freqs = (\x -> freq * (2 ** (x / 1200))) <$> cents
        fades = (\x -> exp $ fade * abs x) <$> tunes
        wides = zipWith (*) (cycle [1, -1]) $ (*wide) <$> (/ fromIntegral predN) <$> fromIntegral <$> [0..predN]
    monos <- oscs -< freqs
    let waves = zipWith pan wides $ zipWith (*) fades monos
    returnA -< sum waves * mono (recip $ sum fades)
