-- IIR filters

{-# LANGUAGE Arrows, Strict, StrictData #-}

module IIR(Biquad(..), biquad, lp1, lp2, hp1, hp2) where
import Prelude hiding ((.))
import Control.Category
import Control.Arrow
import Instrument
import Stereo

data Biquad = Biquad {
    bqW0 :: Double, -- Center frequency (rad/s)
    bqA :: Double, -- Numerator of the transfer function
    bqB :: Double, -- (in decreasing order)
    bqC :: Double,
    bqU :: Double, -- Denominator of the transfer function
    bqV :: Double, -- (in decreasing order)
    bqW :: Double
  }

biquad :: Inst (Biquad, Double) Double
biquad = feedback (0, 0) $ proc ((params, x), (w1, w2)) -> do
  sampRate <- sampFreq -< ()
  let k = bqW0 params / tan (bqW0 params / sampRate / 2)
      a = bqA params * k * k
      b = bqB params * k
      c = bqC params
      u = bqU params * k * k
      v = bqV params * k
      w = bqW params
      scale = u + v + w
      a' = a + b + c
      b' = 2 * (c - a)
      c' = a - b + c
      u' = 2 * (w - u)
      v' = u - v + w
      w0 = x - (u' * w1 + v' * w2) / scale
      y0 = (a' * w0 + b' * w1 + c' * w2) / scale
  returnA -< (y0, (w0, w1))

-- Low Pass I
lp1 :: Inst (Double, Double) Double
lp1 = proc (freq, x) -> do
  let w = 2 * pi * freq
  biquad -< (Biquad w 0 0 w 0 1 w, x)

-- High Pass I
hp1 :: Inst (Double, Double) Double
hp1 = proc (freq, x) -> do
  let w = 2 * pi * freq
  biquad -< (Biquad w 0 1 0 0 1 w, x)

-- Low Pass II
lp2 :: Inst ((Double, Double), Double) Double
lp2 = proc ((freq, q), x) -> do
  let wn = 2 * pi * freq
      zeta = 1 / 2 / q
  biquad -< (Biquad wn 0 0 (wn*wn) 1 (2*zeta*wn) (wn*wn), x)

-- High Pass II
hp2 :: Inst ((Double, Double), Double) Double
hp2 = proc ((freq, q), x) -> do
  let wn = 2 * pi * freq
      zeta = 1 / 2 / q
  biquad -< (Biquad wn 1 0 0 1 (2*zeta*wn) (wn*wn), x)
