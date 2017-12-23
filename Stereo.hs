-- Data type for stereo sample

{-# LANGUAGE Strict, StrictData #-}

module Stereo(Stereo(..), dB, mono, pan) where

data Stereo = Stereo {lChn :: Double, rChn :: Double} deriving Show

instance Num Stereo where
  (Stereo a b) + (Stereo c d) = Stereo (a + c) (b + d)
  (Stereo a b) * (Stereo c d) = Stereo (a * c) (b * d)
  abs (Stereo a b) = Stereo (abs a) (abs b)
  signum (Stereo a b) = Stereo (signum a) (signum b)
  fromInteger x = mono $ fromInteger x
  negate (Stereo a b) = Stereo (negate a) (negate b)

-- Decibel to linear scale
dB x = 10 ** (x / 20)

-- Convert mono sample to stereo sample by panning
pan :: Double -> Double -> Stereo
pan angle x =
  let
    c = cos angle
    s = sin angle
    scale = sqrt 2 / 2
  in
    Stereo (scale * (c + s) * x) (scale * (c - s) * x)

-- Convert mono sample to stereo sample by copying
mono x = Stereo x x
