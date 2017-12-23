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

evRatio = put <$> ratio

-- Note
data PitchBox = forall a. Pitch a => PitchBox a

instance Pitch PitchBox where
  getPitch k m (PitchBox p) = getPitch k m p

note = (PitchBox <$> absoluteNote)
  <|> (PitchBox <$> interval)
  <|> (PitchBox <$> scaleDegree)

evNote = do
  p <- note
  return $ do
    duration <- get
    lift $ M.note duration p

rest = do
  char '.'
  return $ do
    duration <- get
    lift $ M.rest duration

-- Chord
chord = do
  char '('
  notes <- sepBy1 note spaces
  char ')'
  return $ do
    duration <- get
    lift $ forM notes $ M.tone duration
    lift $ M.rest duration

-- Transpose
up = (lift $ M.transpose (True, Interval IPerfect 8)) <$ char '^'
down = (lift $ M.transpose (False, Interval IPerfect 8)) <$ char 'v'

evTranspose = do
  offset <- transpose
  return $ lift $ M.transpose offset

-- Event
event = try evRatio <|> evNote <|> rest <|> up <|> down <|> chord <|> evTranspose
events = foldl (>>) (return ()) <$> sepBy1 event spaces

-- Exported function
music x = evalStateT (parseString events x) 1
