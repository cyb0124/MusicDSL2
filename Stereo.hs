-- Data type for stereo sample

module Stereo(Stereo(..), dB, mono, pan) where

data Stereo = Stereo {lChn :: Float, rChn :: Float} deriving Show

-- Decibel to linear scale
dB x = 10 ** (x / 20)

-- Convert mono sample to stereo sample by panning
pan :: Float -> Float -> Stereo
pan angle x =
  let
    c = cos angle
    s = sin angle
    scale = sqrt 2 / 2
  in
    Stereo (scale * (c + s) * x) (scale * (c - s) * x)

-- Panning at center
mono = pan 0
