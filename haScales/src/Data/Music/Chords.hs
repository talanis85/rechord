{-# LANGUAGE ExistentialQuantification #-}
module Data.Music.Chords
    (
    -- * Chord definition
      Chord (Chord, chordScale, chordNotes, chordSlash), mkChord, (</>)
    , with, without, chordHas
    , chordDegrees
    , chordName
    -- * Triads, tetrads and pentachords
    , triad, tetrad, pentachord
    -- * Common chords
    , majorChord, major7Chord, dominant7Chord, minorChord, minor7Chord, minor7b5Chord, minorMajor7Chord
    , major9Chord, minor9Chord, dominant9Chord
    , diminishedChord, augmentedChord
    , susChord, sus2Chord, sus4Chord, sus7Chord
    , major6Chord, minor6Chord, major69Chord, minor69Chord
    -- * Common chord modifications
    , applySus2, applySus4, applySus
    ) where

import Data.Monoid
import Data.Maybe
import Data.List (intersperse)
import Data.Set hiding (map, filter)
import qualified Data.Set as S

import Data.Music.Scales

-- | A chord is defined by a scale and a list of degrees.
data Chord = Chord
    { chordScale :: Scale
    , chordNotes :: Set Degree
    , chordSlash :: Degree
    }
  deriving (Eq)

mkChord :: Scale -> [Degree] -> Chord
mkChord s l = Chord s (fromList l) (Degree I natural)

(</>) :: Chord -> Degree -> Chord
c </> d = c { chordSlash = d }

with :: Chord -> Degree -> Chord
with (Chord s l sl) d = Chord s (insert d l) sl

without :: Chord -> DegreeBase -> Chord
without (Chord s l sl) d = Chord s (S.filter (not . isDegreeBase d) l) sl
    where isDegreeBase d (Degree d' _) = d == d'

chordHas :: Chord -> Degree -> Bool
chordHas (Chord s l sl) d = S.member d l

-- | Read a chord's degrees.
chordDegrees :: Chord -> Set Degree
chordDegrees (Chord s l sl) = S.map (degreeN s) l

-- TODO: Prove that
--      forall s l: size chordDegrees (Chord s l sl) == size l


degreeFunction :: Degree -> String
degreeFunction (Degree I    (Accidental 0)) = "1"
degreeFunction (Degree II   (Accidental 0)) = "9"
degreeFunction (Degree III  (Accidental 0)) = "3"
degreeFunction (Degree IV   (Accidental 0)) = "11"
degreeFunction (Degree V    (Accidental 0)) = "5"
degreeFunction (Degree VI   (Accidental 0)) = "13"
degreeFunction (Degree VII  (Accidental 0)) = "maj7"
degreeFunction (Degree VII  (Accidental (-1))) = "7"
degreeFunction (Degree d    m) = (show m) ++ (degreeFunction $ Degree d (Accidental 0))


-- | A triad.
triad :: Scale -> Chord
triad s = mkChord s [Degree I natural, Degree III natural, Degree V natural]

-- | A tetrad.
tetrad :: Scale -> Chord
tetrad s = mkChord s [Degree I natural, Degree III natural, Degree V natural, Degree VII natural]

-- | A pentachord.
pentachord :: Scale -> Chord
pentachord s = mkChord s [Degree I natural, Degree III natural, Degree V natural, Degree VII natural, Degree II natural]

majorChord = triad ionianScale
major7Chord = tetrad ionianScale
dominant7Chord = tetrad mixolydianScale
minorChord = triad aeolianScale
minor7Chord = tetrad aeolianScale
minor7b5Chord = tetrad locrianScale
minorMajor7Chord = tetrad melodicMinorScale
diminishedChord = triad locrianScale
augmentedChord = triad ionianAugmentedScale

major9Chord = pentachord ionianScale
minor9Chord = pentachord aeolianScale
dominant9Chord = pentachord mixolydianScale

applySus2 c = c `without` III `with` Degree II natural
applySus4 c = c `without` III `with` Degree IV natural
applySus = applySus2 . applySus4

susChord = mkChord ionianScale [Degree I natural, Degree II natural, Degree IV natural]
sus2Chord = mkChord ionianScale [Degree I natural, Degree II natural, Degree V natural]
sus4Chord = mkChord ionianScale [Degree I natural, Degree IV natural, Degree V natural]
sus7Chord = mkChord mixolydianScale [Degree I natural, Degree II natural, Degree IV natural, Degree VII natural]

major6Chord = mkChord ionianScale [Degree I natural, Degree III natural, Degree VI natural]
minor6Chord = mkChord dorianScale [Degree I natural, Degree III natural, Degree VI natural]
major69Chord = mkChord ionianScale [Degree I natural, Degree III natural, Degree VI natural, Degree II natural]
minor69Chord = mkChord dorianScale [Degree I natural, Degree III natural, Degree VI natural, Degree II natural]






instance Show Chord where
    show c = show $ chordDegrees c

matchWithRemainder :: (Ord a) => Set a -> Set a -> Maybe (Set a)
matchWithRemainder a b
    | b `isSubsetOf` a  = Just $ a `difference` b
    | otherwise         = Nothing

matchWithRemainder' d name m = case matchWithRemainder d m of
    Nothing -> First Nothing
    Just rem -> First $ Just (name, rem)

chordType d = getFirst $ mconcat $ map (matchType d) chordTypes
  where
    matchType d (l, opt, name) = First $ matchWithRemainder d (fromList l) >>= \rem -> return (rem, opt, name)

data ChordType = NormalChord | DominantChord | SusChord

chordTypes =
    [ ([Degree III natural, Degree VII flat], DominantChord, "7")
    , ([Degree III flat, Degree VII natural], NormalChord, "m|maj7")
    , ([Degree III flat, Degree VII flat], NormalChord, "m7")
    , ([Degree III natural, Degree VII natural], NormalChord, "maj7")
    , ([Degree III flat, Degree V flat], NormalChord, "dim")
    , ([Degree III natural, Degree V sharp], NormalChord, "+")
    , ([Degree II natural, Degree III natural, Degree VI natural], NormalChord, "69")
    , ([Degree III natural, Degree VI natural], NormalChord, "6")
    , ([Degree III flat, Degree VI natural], NormalChord, "m6")
    , ([Degree III flat], NormalChord, "m")
    , ([Degree III natural], NormalChord, "")
    , ([Degree II natural, Degree IV natural, Degree VII flat], SusChord, "7sus")
    , ([Degree II natural, Degree VII flat], SusChord, "7sus2")
    , ([Degree IV natural, Degree VII flat], SusChord, "7sus4")
    , ([Degree II natural, Degree IV natural], SusChord, "sus")
    , ([Degree II natural], SusChord, "sus2")
    , ([Degree IV natural], SusChord, "sus4")
    , ([], SusChord, "sus")
    ]

normalOptions =
    [ (Degree II flat, "b9")
    , (Degree II natural, "add9")
    , (Degree II sharp, "#9")
    , (Degree IV flat, "b11")
    , (Degree IV natural, "11")
    , (Degree IV sharp, "#11")
    , (Degree V flat, "b5")
    , (Degree V sharp, "#5")
    , (Degree VI flat, "b6")
    , (Degree VI natural, "6")
    , (Degree VI sharp, "#6")
    ]

dominantOptions =
    [ (Degree II flat, "b9")
    , (Degree II natural, "9")
    , (Degree II sharp, "#9")
    , (Degree IV flat, "b11")
    , (Degree IV natural, "11")
    , (Degree IV sharp, "#11")
    , (Degree V flat, "b5")
    , (Degree V sharp, "#5")
    , (Degree VI flat, "b13")
    , (Degree VI natural, "13")
    , (Degree VI sharp, "#13")
    ]

susOptions =
    [ (Degree II flat, "b9")
    , (Degree II natural, "2")
    , (Degree II sharp, "#9")
    , (Degree IV flat, "b11")
    , (Degree IV natural, "4")
    , (Degree IV sharp, "#11")
    , (Degree V flat, "b5")
    , (Degree V sharp, "#5")
    , (Degree VI flat, "b6")
    , (Degree VI natural, "6")
    , (Degree VI sharp, "#6")
    ]

chordName c =
    let d = chordDegrees c
    in case chordType d of
        Nothing -> "?"
        Just (rem, NormalChord, name) -> name ++ chordOptions normalOptions rem
        Just (rem, DominantChord, name) -> name ++  chordOptions dominantOptions rem
        Just (rem, SusChord, name) -> name ++ chordOptions susOptions rem

chordOptions table d = concat $ mapMaybe (matchOption d) table
    where matchOption d (o, n) = if o `S.member` d then Just ("|" ++ n) else Nothing
