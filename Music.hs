-- This file defines the musical notation eDSL
-- For the instrument specification language, see Instrument.hs

module Music(
  Time, Music, key, mode, bpm, transpose, rest, tone, note, inst,
  duration, Event(..), TimedEvent(..), BPMChange(..), Note(..), getKey,
  diatonicTranspose, compileMusic, (<:>), getTime, getBPM, scoped, modal,
  mapNote, reGate, getMode, reInst, reProp, arpeggio, sustain, simulate
) where
import Control.Monad.Writer.Lazy
import Control.Monad.State.Lazy
import Control.Arrow
import Data.Functor.Identity
import Data.Monoid
import Data.Ratio
import Data.List (sortOn, elemIndex)
import Instrument
import Stereo
import Theory
import Parser hiding (transpose)

-- The type of musical time (number of beats)
type Time = Ratio Int

type MusicWriterT = WriterT [TimedEvent]
data TimedEvent = TimedEvent {teTime :: Time, teEvent :: Event}
data Event = EvBPMChange BPMChange | EvNote Note
data BPMChange = BPMChange Double deriving Show
data Note = Note {
    nInst :: Inst () Stereo, -- The instrument used for this note
    nPitch :: Int,           -- The pitch of this note (number of semitones from A4)
    nDuration :: Time        -- The duration of this note
  }

-- The music monad contains a writer for recording the notes and a state for
-- keep tracking of the local state.
type MusicStateT = StateT MusicState
data MusicState = MusicState {
    nsTime :: Time,          -- Current time
    nsKey :: Int,            -- Current key
    nsMode :: Mode,          -- Current mode
    nsBPM :: Double,         -- Current tempo
    nsDuration :: Time,      -- Current note duration
    nsInst :: Inst () Stereo -- Current instrument
  }

type Music = MusicWriterT (MusicStateT Identity)

tellTimed :: Event -> Music ()
tellTimed x = do
  time <- nsTime <$> get
  tell [TimedEvent time x]

key :: ToAbsoluteNote a => a -> Music ()
key x = do
  now <- get
  put $ now {nsKey = toAbsoluteNote x}

mode :: Mode -> Music ()
mode x = do
  now <- get
  put $ now {nsMode = x}

inst :: Inst () Stereo -> Music ()
inst x = do
  now <- get
  put $ now {nsInst = x}

bpm :: Double -> Music ()
bpm x = do
  now <- get
  put $ now {nsBPM = x}
  tellTimed $ EvBPMChange $ BPMChange x

duration :: Time -> Music ()
duration x = do
  now <- get
  put $ now {nsDuration = x}

transpose :: ToTranspose a => a -> Music ()
transpose x = do
  now <- get
  put $ now {nsKey = nsKey now + toTranspose x}

getTime :: Music Time
getTime = nsTime <$> get

getBPM :: Music Double
getBPM = nsBPM <$> get

getMode :: Music Mode
getMode = nsMode <$> get

getKey :: Music Int
getKey = nsKey <$> get

rest :: Music ()
rest = do
  now <- get
  put now {nsTime = nsTime now + nsDuration now}

-- same as note but without delay
tone :: Pitch p => p -> Music ()
tone p = do
  now <- get
  let pitch = getPitch (nsKey now) (nsMode now) p
  tellTimed $ EvNote $ Note (nsInst now) pitch (nsDuration now)

-- same as tone but with delay
note :: Pitch p => p -> Music ()
note p = tone p >> rest

-- produce the final event list
compileMusic :: Music () -> [TimedEvent]
compileMusic m =
  let
    s = execWriterT m
    e = runIdentity $ evalStateT s $ MusicState {
        nsTime = 0,
        nsKey = 0,
        nsMode = Major,
        nsBPM = 128,
        nsDuration = 1/1,
        nsInst = arr $ const $ Stereo 0 0
      }
  in sortOn teTime e

-- combines two pieces of music parallelly
(<:>) :: Music () -> Music () -> Music ()
x1 <:> x2 = do
  s <- get
  let y1 = execWriterT x1
      y2 = execWriterT x2
      (e1, s1) = runIdentity $ runStateT y1 s
      (e2, s2) = runIdentity $ runStateT y2 s
      time = nsTime s1 `max` nsTime s2
  tell e1
  tell e2
  put $ s1 {nsTime = time}

-- restores all states except nsTime when the scope is exited
scoped :: Music a -> Music a
scoped x = do
  s <- get
  y <- x
  time <- getTime
  put $ s {nsTime = time}
  return y

-- temporarily change the mode
modal :: Mode -> Music a -> Music a
modal m x = do
  old <- getMode
  mode m
  y <- x
  mode old
  return y

-- Apply note transformation to note event
mapNote :: (Note -> Note) -> TimedEvent -> TimedEvent
mapNote f (TimedEvent time (EvNote n)) = TimedEvent time $ EvNote $ f n
mapNote _ other = other

-- change some properties of all notes in a piece of music
reProp :: (Note -> Note) -> Music a -> Music a
reProp f = censor (mapNote f <$>)

-- change the duration of all notes in a piece of music
reGate :: (Time -> Time) -> Music a -> Music a
reGate f = reProp (\n -> n {nDuration = f $ nDuration n})

-- change the instrument of all notes in a piece of music
reInst :: (Inst () Stereo -> Inst () Stereo) -> Music a -> Music a
reInst f = reProp (\n -> n {nInst = f $ nInst n})

-- transpose a piece of music by a scale interval
-- (useful for making chord progressions, parallel thirds, etc.)
diatonicTranspose :: Int -> Music a -> Music a
diatonicTranspose offset m = do
  nowKey <- getKey
  nowMode <- getMode
  let nowScale = getScale nowMode
      mapToScale x =
        let
          relative = x - nowKey
          octave = relative `div` 12
          pitch = relative `mod` 12
          Just scaleDegree = elemIndex pitch nowScale
        in octave * (length nowScale) + scaleDegree
      mapToNote x =
        let
          octave = x `div` (length nowScale)
          scaleDegree = x `mod` (length nowScale) + 1
        in getPitch nowKey nowMode (ScaleDegree 0 scaleDegree) + 12 * octave
      mapPitch x =
        mapToNote $ (+offset) $ mapToScale $ x
  reProp (\n -> n {nPitch = mapPitch $ nPitch n}) m

-- Arpeggio
arpeggio :: Time -> Music a -> Music a
arpeggio t x =
  let
    f i = \x -> case x of
      TimedEvent nTime (EvNote n) -> TimedEvent (nTime + t * i) $ EvNote $
        n {nDuration = nDuration n - t * i}
      other -> other
  in censor (zipWith f [0..]) x

-- Evaluate a music monad without producing any events or changing the state
simulate :: Music a -> Music a
simulate x = do
  s <- get
  return $ runIdentity $ evalStateT (fst <$> runWriterT x) s

-- Sustain pedal
sustain :: Music a -> Music a
sustain x = do
  endTime <- simulate $ x >> getTime
  let f (TimedEvent nTime (EvNote n)) = TimedEvent nTime $ EvNote $
        n {nDuration = endTime - nTime}
      f other = other
  censor (f <$>) x
