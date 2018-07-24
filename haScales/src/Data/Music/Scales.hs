module Data.Music.Scales
    ( Degree (Degree)
    , DegreeBase (I, II, III, IV, V, VI, VII)
    , degreeNum, degreePred
    , Accidental (Accidental), (<+>), accMinus, natural, flat, sharp
    , scaleHas
    -- * Scales
    , Scale (scale), degreeN
    -- ** The ionian system
    , ionian
    , ionianScale, dorianScale, phrygianScale, lydianScale, mixolydianScale, aeolianScale, locrianScale
    -- ** The harmonic minor system
    , harmonicMinor
    , harmonicMinorScale, locrianSharp6Scale, ionianAugmentedScale, romanianScale
    , phrygianDominantScale, lydianSharp2Scale, ultralocrianScale
    -- ** The melodic minor system
    , melodicMinor
    , melodicMinorScale
    , pentatonic
    ) where

import Data.Music.Aux

-- | A Degree is a number from I to VII with an optional accidental.
data Degree = Degree DegreeBase Accidental
    deriving (Eq)

instance Ord Degree where
    compare (Degree b1 a1) (Degree b2 a2) =
        case compare b1 b2 of
            EQ -> compare a1 a2
            r  -> r

instance Show Degree where
    show (Degree d m) = (show d) ++ (show m)

data DegreeBase = I | II | III | IV | V | VI | VII
    deriving (Show, Enum, Bounded, Ord, Eq)

-- | Map degrees to natural numbers
degreeNum I = 0
degreeNum II = 1
degreeNum III = 2
degreeNum IV = 3
degreeNum V = 4
degreeNum VI = 5
degreeNum VII = 6

-- | Map degrees to semitones from root
degreeInterval I natural = 0
degreeInterval II natural = 2
degreeInterval III natural = 4
degreeInterval IV natural = 5
degreeInterval V natural = 7
degreeInterval VI natural = 9
degreeInterval VII natural = 11
degreeInterval b a | a > natural = (degreeInterval b (a <+> flat)) + 1
                   | a < natural = (degreeInterval b (a <+> sharp)) - 1

-- | Get the predecessor of a degree. Returns a tuple (a,b) where
--      a is the preceding degree and
--      b denotes if it was a halftone step
degreePred I = (VII, True)
degreePred II = (I, False)
degreePred III = (II, False)
degreePred IV = (III, True)
degreePred V = (IV, False)
degreePred VI = (V, False)
degreePred VII = (VI, False)

-- | Accidental(s) for a degree.
data Accidental = Accidental Int
    deriving (Eq, Ord)

-- | Addition of accidentals
(<+>) :: Accidental -> Accidental -> Accidental
(Accidental a) <+> (Accidental b) = Accidental (a + b)

accMinus :: Accidental -> Accidental -> Accidental
(Accidental a) `accMinus` (Accidental b) = Accidental (a - b)

instance Show Accidental where
    show (Accidental n) | n == 0 = ""
                       | n > 0  = '#' : (show $ Accidental $ n-1)
                       | n < 0  = 'b' : (show $ Accidental $ n+1)

natural = Accidental 0
sharp = Accidental 1
flat = Accidental $ -1

-- | Normalize a scale. After this operation, the scale's first member is always:
--   Degree I natural.
normalize :: [Degree] -> [Degree]
normalize [] = []
normalize (l:ls) = case l of
    Degree I (Accidental m) -> map (normalizeMod (Accidental $ 0 - m)) (l:ls)
    Degree d _ -> let (pred, ht) = degreePred d
                  in normalize $ map (decreaseOne ht) (l:ls)
  where
        normalizeMod m' (Degree d m) = Degree d (m <+> m')
        decreaseOne ht (Degree d m) = let (pred, ht') = degreePred d
                                          mod         = case (ht, ht') of
                                                            (True, True)    -> natural
                                                            (False, False)  -> natural
                                                            (False, True)   -> flat
                                                            (True, False)   -> sharp
                                      in Degree pred (m <+> mod)

data Scale = Scale { scale :: [Degree] }
    deriving (Eq)

degreeN :: Scale -> Degree -> Degree
degreeN s (Degree d m) = let s' = scale s
                             Degree d' m' = s' !! ((degreeNum d) `mod` (length s'))
                         in Degree d' (m <+> m')

scaleHas :: Scale -> Degree -> Bool
scaleHas s d = d `elem` scale s

-- | Left-rotate a list.
rotate :: Int -> [a] -> [a]
rotate 0 l = l
rotate n l = rotate (n-1) (shift l)
  where
    shift :: [a] -> [a]
    shift (x:xs) = xs ++ [x]

ionian n = Scale $ normalize $ rotate n
    [ Degree I natural
    , Degree II natural
    , Degree III natural
    , Degree IV natural
    , Degree V natural
    , Degree VI natural
    , Degree VII natural
    ]

ionianScale =     ionian 0
dorianScale =     ionian 1
phrygianScale =   ionian 2
lydianScale =     ionian 3
mixolydianScale = ionian 4
aeolianScale =    ionian 5
locrianScale =    ionian 6

harmonicMinor n = Scale $ normalize $ rotate n
    [ Degree I natural
    , Degree II natural
    , Degree III flat
    , Degree IV natural
    , Degree V natural
    , Degree VI flat
    , Degree VII natural
    ]

harmonicMinorScale =    harmonicMinor 0
locrianSharp6Scale =    harmonicMinor 1
ionianAugmentedScale =  harmonicMinor 2
romanianScale =         harmonicMinor 3
phrygianDominantScale = harmonicMinor 4
lydianSharp2Scale =     harmonicMinor 5
ultralocrianScale =     harmonicMinor 6

melodicMinor n = Scale $ normalize $ rotate n
    [ Degree I natural
    , Degree II natural
    , Degree III flat
    , Degree IV natural
    , Degree V natural
    , Degree VI natural
    , Degree VII natural
    ]

melodicMinorScale =     melodicMinor 0
dorianB2Scale =         melodicMinor 1
lydianAugmentedScale =  melodicMinor 2
acousticScale =         melodicMinor 3
hinduScale =            melodicMinor 4
locrianSharp9Scale =    melodicMinor 5
superlocrianScale =     melodicMinor 6

pentatonic n = Scale $ normalize $ rotate n
    [ Degree I natural
    , Degree II natural
    , Degree III natural
    , Degree V natural
    , Degree VI natural
    ]

majorPentaScale     = pentatonic 0
penta2Scale         = pentatonic 1
penta3Scale         = pentatonic 2
penta4Scale         = pentatonic 3
minorPentaScale     = pentatonic 4
