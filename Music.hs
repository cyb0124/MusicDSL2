-- This file defines the musical notation eDSL
-- For the instrument specification language, see Instrument.hs

module Music(
  Time, Music, key, mode, bpm, transpose, rest, tone, note, inst, duration,
  Event(..), TimedEvent(..), BPMChange(..), Note(..),
  compileMusic, (<:>), getTime, getBPM, scoped, modal, reGate, getMode
) where
import Control.Monad.Writer.Lazy
import Control.Monad.State.Lazy
import Control.Arrow
import Data.Functor.Identity
import Data.Monoid
import Data.Ratio
import Data.List (sortOn)
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
    nInst :: InstProc, -- The instrument used for this note
    nPitch :: Int,     -- The pitch of this note (number of semitones from A4)
    nDuration :: Time  -- The duration of this note
  }

-- The music monad contains a writer for recording the notes and a state for
-- keep tracking of the local state.
type MusicStateT = StateT MusicState
data MusicState = MusicState {
    nsTime :: Time,     -- Current time
    nsKey :: Int,       -- Current key
    nsMode :: Mode,     -- Current mode
    nsBPM :: Double,    -- Current tempo
    nsDuration :: Time, -- Current note duration
    nsInst :: InstProc  -- Current instrument
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
  put $ now {nsInst = compileInst x}

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
        nsInst = compileInst $ arr $ const $ Stereo 0 0
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

-- change the duration of all notes in a piece of music
reGate :: (Time -> Time) -> Music a -> Music a
reGate f x =
  let
    f' (TimedEvent time (EvNote n)) =
      TimedEvent time $ EvNote $ n {nDuration = f $ nDuration n}
  in do
    (y, notes) <- lift $ runWriterT x
    tell $ f' <$> notes
    return y
