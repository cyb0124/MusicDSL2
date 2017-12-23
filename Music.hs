-- This file defines the musical notation eDSL
-- For the instrument specification language, see Instrument.hs

{-# LANGUAGE Arrows #-}

module Music(
  Time, Music, key, mode, bpm, transpose, rest, tone, note, inst,
  Event(..), TimedEvent(..), BPMChange(..), Note(..),
  compileMusic
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
    nsTime :: Time,    -- Current time
    nsKey :: Int,      -- Current key
    nsMode :: Mode,    -- Current mode
    nsBPM :: Double,   -- Current tempo
    nsInst :: InstProc -- Current instrument
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

transpose :: ToTranspose a => a -> Music ()
transpose x = do
  now <- get
  put $ now {nsKey = nsKey now + toTranspose x}

rest :: Time -> Music ()
rest duration = do
  now <- get
  put now {nsTime = nsTime now + duration}

-- same as note but without delay
tone :: Pitch p => Time -> p -> Music ()
tone duration p = do
  now <- get
  let pitch = getPitch (nsKey now) (nsMode now) p
  tellTimed $ EvNote $ Note (nsInst now) pitch duration

-- same as tone but with delay
note :: Pitch p => Time -> p -> Music ()
note duration p = do
  tone duration p
  rest duration

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
        nsInst = compileInst $ proc () -> do returnA -< Stereo 0 0
      }
  in sortOn teTime e
