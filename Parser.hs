-- Parser for miscellaneous strings

module Parser() where
import Text.Parsec.Char
import Text.Parsec

-- Accidentals
sharp = 1 <$ char '#'
flat = -1 <$ char 'b'
accidental = sharp <|> flat
accidentals = sum <$> many accidental

-- Absolute notes
noteLetter = (-fromEnum 'A') <$> fromEnum <$> satisfy $ \x -> x >= 'A' && x <= 'G'
octaveNum = (-fromEnum '4') <$> fromEnum <$> digit
absoluteNote = sum <$> sequence [noteLetter, accidentals, (*12) <$> octaveNum]
