-- Parser for miscellaneous musical notation strings

{-# LANGUAGE FlexibleInstances #-}

module Parser(
  StringParser, parseString,
  ToAbsoluteNote(..), ToTranspose(..), ToScaleDegree(..), Pitch(..),
  transpose, absoluteNote, interval, scaleDegree
) where
import Data.Functor.Identity
import Text.Parsec.Char
import Text.Parsec
import Theory

type StringParser = ParsecT String () Identity
parseString :: StringParser a -> String -> a
parseString p x = case parse p [] x of
  Left e -> error $ "Failed to parse " ++ show x ++ ": " ++ show e
  Right y -> y
digitNum = (subtract $ fromEnum '0') <$> fromEnum <$> digit

-- Accidentals
sharp = 1 <$ char '#'
flat = -1 <$ char 'b'
accidental = sharp <|> flat
accidentals = sum <$> many accidental

-- Absolute notes
noteLetter = ([0, 2, 3, 5, 7, 8, 10] !!) <$> (subtract $ fromEnum 'A')
  <$> fromEnum <$> satisfy (\x -> x >= 'A' && x <= 'G')
octaveNum = (subtract 4) <$> digitNum
absoluteNote = sum <$> sequence [noteLetter, accidentals, (*12) <$> octaveNum]

class ToAbsoluteNote a where
  toAbsoluteNote :: a -> Int

instance ToAbsoluteNote Int where
  toAbsoluteNote = id

instance ToAbsoluteNote String where
  toAbsoluteNote = parseString absoluteNote

-- Intervals
major = IMajor <$ char 'M'
minor = IMinor <$ char 'm'
perfect = IPerfect <$ char 'P'
diminished = IDiminished <$ string "dim"
augmented = IAugmented <$ string "Aug"
quality = major <|> minor <|> perfect <|> diminished <|> augmented
interval = Interval <$> quality <*> digitNum
up = id <$ char '+'
down = negate <$ char '-'
transpose = (up <|> down) <*> (fromInterval <$> interval)

class ToTranspose a where
  toTranspose :: a -> Int

instance ToTranspose Int where
  toTranspose = id

instance ToTranspose String where
  toTranspose = parseString transpose

instance ToTranspose (Bool, Interval) where
  toTranspose (True, x) = fromInterval x
  toTranspose (False, x) = -fromInterval x

-- Relative notes
scaleDegree = ScaleDegree <$> accidentals <*> digitNum

class ToScaleDegree a where
  toScaleDegree :: a -> ScaleDegree

instance ToScaleDegree ScaleDegree where
  toScaleDegree = id

instance ToScaleDegree String where
  toScaleDegree = parseString scaleDegree

-- Polymorphic pitches

class Pitch a where
  getPitch :: Int -> Mode -> a -> Int

instance Pitch Int where
  getPitch _ _ = id

instance Pitch Interval where
  getPitch k _ i = k + fromInterval i

instance Pitch ScaleDegree where
  getPitch k m d = k + fromScaleDegree m d

instance Pitch String where
  getPitch k m = parseString $
    (getPitch k m <$> absoluteNote)
    <|> (getPitch k m <$> interval)
    <|> (getPitch k m <$> scaleDegree)
