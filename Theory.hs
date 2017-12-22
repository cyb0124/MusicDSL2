-- Music theory definitions

module Theory(
    Mode(..), getScale,
    IntervalQuality(..), Interval(..), fromInterval,
    ScaleDegree(..), fromScaleDegree
) where

data Mode =
    Major | Minor | Dorian | Phrygian | Lydian | Mixolydian | Locrian
  | HarmMin | MeloMin | PhrDom | DblHarm
  deriving Show

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

data IntervalQuality = IMajor | IMinor | IPerfect | IDiminished | IAugmented deriving Show
data Interval = Interval IntervalQuality Int deriving Show

fromInterval :: Interval -> Int
fromInterval (Interval IMajor 2) = 2
fromInterval (Interval IMajor 3) = 4
fromInterval (Interval IMajor 6) = 9
fromInterval (Interval IMajor 7) = 11
fromInterval (Interval IMinor 2) = 1
fromInterval (Interval IMinor 3) = 3
fromInterval (Interval IMinor 6) = 8
fromInterval (Interval IMinor 7) = 10
fromInterval (Interval IPerfect 1) = 0
fromInterval (Interval IPerfect 4) = 5
fromInterval (Interval IPerfect 5) = 7
fromInterval (Interval IPerfect 8) = 12
fromInterval (Interval IDiminished 2) = 0
fromInterval (Interval IDiminished 3) = 2
fromInterval (Interval IDiminished 4) = 4
fromInterval (Interval IDiminished 5) = 6
fromInterval (Interval IDiminished 6) = 7
fromInterval (Interval IDiminished 7) = 9
fromInterval (Interval IDiminished 8) = 11
fromInterval (Interval IAugmented 1) = 1
fromInterval (Interval IAugmented 2) = 3
fromInterval (Interval IAugmented 3) = 5
fromInterval (Interval IAugmented 4) = 6
fromInterval (Interval IAugmented 5) = 8
fromInterval (Interval IAugmented 6) = 10
fromInterval (Interval IAugmented 7) = 12
fromInterval (Interval IAugmented 8) = 13
fromInterval x = error $ "invalid interval: " ++ show x

data ScaleDegree = ScaleDegree {sdAccidentals :: Int, sdDegree :: Int}

fromScaleDegree :: Mode -> ScaleDegree -> Int
fromScaleDegree mode (ScaleDegree accidentals degree) =
  getScale mode !! (degree - 1) + accidentals
