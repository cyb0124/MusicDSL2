-- Parser for a sequence of musical notations

{-# LANGUAGE ExistentialQuantification #-}

module SequenceParser(music) where
import Control.Monad.State.Lazy
import Text.Parsec.Char
import Text.Parsec
import Data.Ratio
import Parser
import Theory
import qualified Music as M

-- Time/ratio parsing
num = read <$> many1 digit

ratio = do
  n <- num
  char '/'
  d <- num
  return $ n % d

evRatio = M.duration <$> ratio

-- Note
data PitchBox = forall a. Pitch a => PitchBox a

instance Pitch PitchBox where
  getPitch k m (PitchBox p) = getPitch k m p

note = (PitchBox <$> absoluteNote)
  <|> (PitchBox <$> interval)
  <|> (PitchBox <$> scaleDegree)

evNote f = f <$> note

rest = M.rest <$ char '.'

-- Chord
chord = do
  char '('
  notes <- sepBy1 (evNote M.tone <|> up <|> down <|> evTranspose) spaces
  char ')'
  return $ do
    s <- get
    sequence notes
    put s
    M.rest

-- Transpose
up = M.transpose (True, Interval IPerfect 8) <$ char '^'
down = M.transpose (False, Interval IPerfect 8) <$ char 'v'

evTranspose = M.transpose <$> transpose

-- Scoped
evScoped = do
  char '{'
  notes <- M.scoped <$> events
  char '}'
  return notes

-- Event
event = try evRatio <|> evNote M.note <|> rest <|> up <|> down <|> chord <|> evTranspose <|> evScoped
events = foldl (>>) (return ()) <$> sepBy1 event spaces

-- Exported function
music = parseString events
