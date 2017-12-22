-- This file defines the musical notation eDSL
-- For the instrument specification language, see Instrument.hs

module Music(
  Time, Music
) where
import Control.Monad.Writer.Lazy
import Control.Monad.State.Lazy
import Data.Monoid
import Data.Ratio
import Instrument
import Theory

-- The type of musical time (number of beats)
type Time = Ratio Int

type NoteWriter = Writer [Note]
data Note = Note {
    nInst :: InstProc, -- The instrument used for this note
    nTime :: Time,     -- The trigger time of this note
    nDuration :: Time, -- The duration of this note
    nPitch :: Int      -- The pitch of this note (number of semitones from A4)
  } deriving Show

-- The music monad contains a writer for recording the notes and a state for
-- keep tracking of the local state.
type Music = StateT MusicState NoteWriter
data MusicState = MusicState {
    nsTime :: Time,    -- Current time
    nsKey :: Int,      -- Current key
    nsMode :: Mode,    -- Current mode
    nsTempo :: Float,  -- Current tempo
    nsInst :: InstProc -- Current instrument
  }

key :: String -> NoteWriterState ()
key x = do
    now <- get
    put $ now {nsKey = noteToPitch x}

mode :: Mode -> NoteWriterState ()
mode x = do
    now <- get
    put $ now {nsMode = x}

tempo :: Int -> NoteWriterState ()
tempo x = do
    now <- get
    put $ now {nsTempo = x}

data NoteInput = NoteInput {
    niAccidentals :: Int,
    niScaleDegree :: Int,
    niOctave :: Int}
    
niMapChar :: Char -> NoteInput -> NoteInput
niMapChar '#' prev = prev {niAccidentals = niAccidentals prev + 1}
niMapChar 'b' prev = prev {niAccidentals = niAccidentals prev - 1}
niMapChar '+' prev = prev {niOctave = niOctave prev + 1}
niMapChar '-' prev = prev {niOctave = niOctave prev - 1}
niMapChar c prev = let
    x = fromEnum c - fromEnum '1' in
        if x < 0 || x >= 7 then undefined
        else prev {niScaleDegree = x}

parseNoteInput :: String -> NoteInput
parseNoteInput xs = foldr ($) (NoteInput 0 undefined 0) (niMapChar <$> xs)

durationToSeconds :: IntRatio -> NoteWriterState IntRatio
durationToSeconds x = do
    now <- get
    return $ (60 % nsTempo now) * x

note' :: IntRatio -> String -> NoteWriterState ()
note' duration cs = do
    let ni = parseNoteInput cs
    now <- get
    let time = nsTime now
    seconds <- durationToSeconds duration
    let pOctave = niOctave ni * 12
    let pInput = (mconcat $ iterate (map (+12)) scale) !! (niScaleDegree ni + fromEnum (nsMode now))
    let pKey = nsKey now
    let pMode = negate $ modeToTranspose $ nsMode now
    let pAccidentals = niAccidentals ni
    let finalPitch = pOctave + pInput + pKey + pMode + pAccidentals
    tell [Note time seconds finalPitch]

rest :: IntRatio -> NoteWriterState ()
rest duration = do
    now <- get
    seconds <- durationToSeconds duration
    put now {nsTime = nsTime now + seconds}

note :: IntRatio -> String -> NoteWriterState ()
note duration cs = do
    note' duration cs
    rest duration

transpose :: String -> NoteWriterState ()
transpose (dir:cs) = do
    let offset = dir' $ intervalToTranspose cs where
        dir' = case dir of
            '+' -> id
            '-' -> negate
    now <- get
    let newKey = (nsKey now + offset) `mod` 12
    put now {nsKey = newKey}

execNoteWriterState :: NoteWriterState () -> [Note]
execNoteWriterState x = execWriter $ evalStateT x (NoteState 0 undefined undefined undefined)

noteToNoteInput :: Note -> W.NoteInput
noteToNoteInput (Note time duration pitch) = W.NoteInput freq startTime endTime where
    freq = 440 * 2 ** (fromIntegral pitch / 12)
    startTime = realToFrac time
    endTime = realToFrac $ time + duration

outputAudio :: String -> NoteWriterState () -> IO ()
outputAudio fileName ws = do
    let notes = noteToNoteInput <$> execNoteWriterState ws
    let result = W.computeWave notes
    BS.writeFile fileName result
