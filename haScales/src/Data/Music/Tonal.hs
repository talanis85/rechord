module Data.Music.Tonal
    ( DegreeChord (DegreeChord, dchordDegree, dchordChord)
    , TonalChord (TonalChord, tchordRoot, tchordChord)
    , TonalScale (TonalScale, tscaleRoot, tscaleScale)
    , tonalChordScale
    , scaleNotes, scaleDegree
    , Pitch (Pitch)
    , PitchBase (A, B, C, D, E, F, G)
    , bakeChord
    , transposeChord
    , degreeOf
    ) where

import Data.Music.Aux
import Data.Music.Scales
import Data.Music.Chords

data DegreeChord = DegreeChord
    { dchordDegree  :: Degree
    , dchordChord   :: Chord
    }
  deriving (Show)

-- | A pitch consists of a base note and an accidental.
data Pitch = Pitch PitchBase Accidental

instance Show Pitch where
    show (Pitch b m) = (show b) ++ (show m)

data PitchBase = A | B | C | D | E | F | G
    deriving (Show, Read, Eq, Enum, Bounded)


(<~>) :: Pitch -> Degree -> Pitch
p <~> (Degree d m) = let circle = circleOfFifths p
                         baseScale = nthScale circle
                         Pitch p' m' = baseScale !! (degreeNum d)
                     in Pitch p' (m <+> m')

-- | A tonal scale is defined by a root note and a scale.
data TonalScale = TonalScale
    { tscaleRoot :: Pitch
    , tscaleScale :: Scale
    }

instance Show TonalScale where
    show s = show $ scaleNotes s

scaleNotes :: TonalScale -> [Pitch]
scaleNotes (TonalScale p s) = map (p <~>) (scale s)

scaleDegree :: TonalScale -> Degree -> Pitch
scaleDegree (TonalScale p s) d = p <~> degreeN s d

-- | A tonal chord is a chord with a
data TonalChord = TonalChord
    { tchordRoot :: Pitch
    , tchordChord :: Chord
    }

tonalChordScale :: TonalChord -> TonalScale
tonalChordScale (TonalChord root (Chord s l sl)) = TonalScale root s

instance Show TonalChord where
    show (TonalChord p (Chord s l sl)) = case sl of
        (Degree I (Accidental 0)) -> (show p) ++ (chordName (Chord s l sl))
        sl'                       -> (show p) ++ (chordName (Chord s l sl)) ++ "/" ++ (show $ scaleDegree (TonalScale p s) sl)

bakeChord :: TonalScale -> DegreeChord -> TonalChord
bakeChord scale dchord = TonalChord
    { tchordRoot = scaleDegree scale (dchordDegree dchord)
    , tchordChord = dchordChord dchord
    }

rotate :: Int -> [a] -> [a]
rotate 0 l = l
rotate n l = rotate (n-1) (shift l)
  where
    shift :: [a] -> [a]
    shift (x:xs) = xs ++ [x]

-- | Get the nth scale from the circle of fifths.
nthScale :: Int -> [Pitch]
nthScale n = nthScale' n
    where
        nthScale' n | n == 0 = [Pitch C natural, Pitch D natural, Pitch E natural, Pitch F natural,
                               Pitch G natural, Pitch A natural, Pitch B natural]
                    | n > 0  = let (a:b:c:d:e:f:(Pitch p m):rest) = rotate 4 $ nthScale' (n-1)
                               in a:b:c:d:e:f:(Pitch p (m <+> sharp)):rest
                    | n < 0  = let (a:b:c:(Pitch p m):rest) = rotate 3 $ nthScale' (n+1)
                               in a:b:c:(Pitch p (m <+> flat)):rest

-- | Determine the position of a note in the circle of fifths.
circleOfFifths :: Pitch -> Int
circleOfFifths p = let (sign, num) = circleOfFifths' p p
                   in sign * num
    where
        circleOfFifths' (Pitch C (Accidental 0)) _ = (1, 0)
        circleOfFifths' _ (Pitch C (Accidental 0)) = (-1, 0)
        circleOfFifths' p1 p2 = let (sign, num) = circleOfFifths' (fifthDown p1) (fifthUp p2)
                                in (sign, num + 1)

fifthUp (Pitch C a) = Pitch G a
fifthUp (Pitch D a) = Pitch A a
fifthUp (Pitch E a) = Pitch B a
fifthUp (Pitch F a) = Pitch C a
fifthUp (Pitch G a) = Pitch D a
fifthUp (Pitch A a) = Pitch E a
fifthUp (Pitch B a) = Pitch F (a <+> sharp)

fifthDown (Pitch C a) = Pitch F a
fifthDown (Pitch D a) = Pitch G a
fifthDown (Pitch E a) = Pitch A a
fifthDown (Pitch F a) = Pitch B (a <+> flat)
fifthDown (Pitch G a) = Pitch C a
fifthDown (Pitch A a) = Pitch D a
fifthDown (Pitch B a) = Pitch E a

reduceAccidental :: Pitch -> Pitch
reduceAccidental (Pitch b (Accidental a))
    | a == 0 = Pitch b (Accidental a)
    | a > 0  = pitchSucc $ Pitch b (Accidental (a-1))
    | a < 0  = pitchPred $ Pitch b (Accidental (a+1))

pitchSucc (Pitch C (Accidental 0)) = (Pitch C (Accidental 1))
pitchSucc (Pitch C (Accidental 1)) = (Pitch D (Accidental 0))
pitchSucc (Pitch D (Accidental 0)) = (Pitch D (Accidental 1))
pitchSucc (Pitch D (Accidental 1)) = (Pitch E (Accidental 0))
pitchSucc (Pitch E (Accidental 0)) = (Pitch F (Accidental 0))
pitchSucc (Pitch F (Accidental 0)) = (Pitch F (Accidental 1))
pitchSucc (Pitch F (Accidental 1)) = (Pitch G (Accidental 0))
pitchSucc (Pitch G (Accidental 0)) = (Pitch G (Accidental 1))
pitchSucc (Pitch G (Accidental 1)) = (Pitch A (Accidental 0))
pitchSucc (Pitch A (Accidental 0)) = (Pitch A (Accidental 1))
pitchSucc (Pitch A (Accidental 1)) = (Pitch B (Accidental 0))
pitchSucc (Pitch B (Accidental 0)) = (Pitch C (Accidental 0))
pitchSucc p = pitchSucc (reduceAccidental p)

pitchPred (Pitch C (Accidental 0)) = (Pitch B (Accidental 0))
pitchPred (Pitch C (Accidental 1)) = (Pitch C (Accidental 0))
pitchPred (Pitch D (Accidental 0)) = (Pitch C (Accidental 1))
pitchPred (Pitch D (Accidental 1)) = (Pitch D (Accidental 0))
pitchPred (Pitch E (Accidental 0)) = (Pitch D (Accidental 1))
pitchPred (Pitch F (Accidental 0)) = (Pitch E (Accidental 0))
pitchPred (Pitch F (Accidental 1)) = (Pitch F (Accidental 0))
pitchPred (Pitch G (Accidental 0)) = (Pitch F (Accidental 1))
pitchPred (Pitch G (Accidental 1)) = (Pitch G (Accidental 0))
pitchPred (Pitch A (Accidental 0)) = (Pitch G (Accidental 1))
pitchPred (Pitch A (Accidental 1)) = (Pitch A (Accidental 0))
pitchPred (Pitch B (Accidental 0)) = (Pitch A (Accidental 1))
pitchPred p = pitchPred (reduceAccidental p)

-- | Transpose a chord by n semitones.
transposeChord :: Int -> TonalChord -> TonalChord
transposeChord ht (TonalChord p c)
    | ht == 0 = TonalChord p c
    | ht > 0  = transposeChord (ht - 1) $ TonalChord (pitchSucc p) c
    | ht < 0  = transposeChord (ht + 1) $ TonalChord (pitchPred p) c

-- | Get the degree of a pitch in a certain tonal scale.
degreeOf :: TonalScale -> Pitch -> Degree
degreeOf s p = degreeOf' (scaleNotes s) I p
    where
        degreeOf' ((Pitch p m):ps) dn (Pitch p' m')
            | p == p' = Degree dn (m' `accMinus` m)
            | otherwise = degreeOf' ps (next dn) (Pitch p' m')

