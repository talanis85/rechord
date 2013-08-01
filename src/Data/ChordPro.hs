module Data.ChordPro
    ( Chord (Chord, chordPitch, chordType, chordSlash)
    , Pitch (Pitch)
    , PitchBase (A, B, C, D, E, F, G)
    , PitchMod (Natural, Sharp, Flat)
    , Chunk, Line, Paragraph, Markup (NormalMarkup, TitleMarkup)
    , transpose
    ) where

data Chord = Chord
    { chordPitch :: Pitch
    , chordType :: String
    , chordSlash :: Maybe Pitch
    }

instance Show Chord where
    show (Chord p t Nothing) = (show p) ++ t
    show (Chord p t (Just s)) = (show p) ++ t ++ "/" ++ (show s)

data Pitch = Pitch PitchBase PitchMod

instance Show Pitch where
    show (Pitch b m) = (show b) ++ (show m)

data PitchBase = A | B | C | D | E | F | G
    deriving (Show, Read)
data PitchMod = Natural | Sharp | Flat

instance Show PitchMod where
    show Natural = ""
    show Sharp = "#"
    show Flat = "b"

-- File layout

type Paragraph = [Line]
type Line = [Chunk]
type Chunk = (Maybe Chord, Maybe Markup)
data Markup = NormalMarkup String | TitleMarkup String
    deriving (Show)

type Option = (String, String)


pitchSucc (Pitch C Flat)    = (Pitch C Sharp)
pitchSucc (Pitch C Natural) = (Pitch C Sharp)
pitchSucc (Pitch C Sharp)   = (Pitch D Natural)
pitchSucc (Pitch D Flat)    = (Pitch D Natural)
pitchSucc (Pitch D Natural) = (Pitch D Sharp)
pitchSucc (Pitch D Sharp)   = (Pitch E Natural)
pitchSucc (Pitch E Flat)    = (Pitch E Natural)
pitchSucc (Pitch E Natural) = (Pitch F Natural)
pitchSucc (Pitch E Sharp)   = (Pitch F Sharp)
pitchSucc (Pitch F Flat)    = (Pitch F Natural)
pitchSucc (Pitch F Natural) = (Pitch F Sharp)
pitchSucc (Pitch F Sharp)   = (Pitch G Natural)
pitchSucc (Pitch G Flat)    = (Pitch G Natural)
pitchSucc (Pitch G Natural) = (Pitch G Sharp)
pitchSucc (Pitch G Sharp)   = (Pitch A Natural)
pitchSucc (Pitch A Flat)    = (Pitch A Natural)
pitchSucc (Pitch A Natural) = (Pitch A Sharp)
pitchSucc (Pitch A Sharp)   = (Pitch B Natural)
pitchSucc (Pitch B Flat)    = (Pitch B Natural)
pitchSucc (Pitch B Natural) = (Pitch C Natural)
pitchSucc (Pitch B Sharp)   = (Pitch C Sharp)



pitchPred (Pitch C Flat)    = (Pitch B Flat)
pitchPred (Pitch C Natural) = (Pitch B Natural)
pitchPred (Pitch C Sharp)   = (Pitch C Natural)
pitchPred (Pitch D Flat)    = (Pitch C Natural)
pitchPred (Pitch D Natural) = (Pitch D Flat)
pitchPred (Pitch D Sharp)   = (Pitch D Natural)
pitchPred (Pitch E Flat)    = (Pitch D Natural)
pitchPred (Pitch E Natural) = (Pitch E Flat)
pitchPred (Pitch E Sharp)   = (Pitch E Natural)
pitchPred (Pitch F Flat)    = (Pitch E Flat)
pitchPred (Pitch F Natural) = (Pitch E Natural)
pitchPred (Pitch F Sharp)   = (Pitch F Natural)
pitchPred (Pitch G Flat)    = (Pitch F Natural)
pitchPred (Pitch G Natural) = (Pitch G Flat)
pitchPred (Pitch G Sharp)   = (Pitch G Natural)
pitchPred (Pitch A Flat)    = (Pitch G Natural)
pitchPred (Pitch A Natural) = (Pitch A Flat)
pitchPred (Pitch A Sharp)   = (Pitch A Natural)
pitchPred (Pitch B Flat)    = (Pitch A Natural)
pitchPred (Pitch B Natural) = (Pitch B Flat)
pitchPred (Pitch B Sharp)   = (Pitch B Natural)


transpose :: Int -> [Paragraph] -> [Paragraph]
transpose amt = map . map . map $ transposeChunk amt

{-
transposeParagraph :: Int -> Paragraph -> Paragraph
transposeParagraph amt = map $ transposeLine amt

transposeLine :: Int -> Line -> Line
transposeLine amt = map $ transposeChunk amt
-}

transposeChunk :: Int -> Chunk -> Chunk
transposeChunk amt (Just c, x) = (Just (transposeChord amt c), x)
transposeChunk _ ch = ch

transposeChord :: Int -> Chord -> Chord
transposeChord amt (Chord p t Nothing) = Chord (transposePitch amt p) t Nothing
transposeChord amt (Chord p t (Just s)) = Chord (transposePitch amt p) t (Just $ transposePitch amt s)

transposePitch :: Int -> Pitch -> Pitch
transposePitch amt p
    | amt == 0 = p
    | amt > 0  = foldr (.) id (replicate (abs amt) pitchSucc) p
    | amt < 0  = foldr (.) id (replicate (abs amt) pitchPred) p

