-- Music theory definitions

module Theory(
    Mode(..), getScale
) where

data Mode =
    Major | Minor | Dorian | Phrygian | Lydian | Mixolydian | Locrian
  | HarmMin | MeloMin | PhrDom | DblHarm
  deriving (Show, Enum)

getScale :: Mode -> [Int]
getScale Dorian     = [0, 2, 3, 5, 7, 9, 10]
getScale Minor      = [0, 2, 3, 5, 7, 8, 10]
getScale Phrygian   = [0, 1, 3, 5, 7, 8, 10]
getScale Lydian     = [0, 2, 4, 6, 7, 9, 11]
getScale Major      = [0, 2, 4, 5, 7, 9, 11]
getScale Mixolydian = [0, 2, 4, 5, 7, 9, 10]
getScale Locrian    = [0, 1, 3, 5, 6, 8, 10]
getScale HarmMin    = [0, 2, 3, 5, 7, 8, 11] -- Harmonic Minor
getScale MeloMin    = [0, 2, 3, 5, 7, 9, 11] -- Melodic Minor
getScale PhrDom     = [0, 1, 4, 5, 7, 8, 10] -- Phrygian Dominant
getScale DblHarm    = [0, 1, 4, 5, 7, 8, 11] -- Double Harmonic Major
