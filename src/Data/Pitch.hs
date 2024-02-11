module Data.Pitch
  ( Pitch (..)
  , PitchClass (..)
  , PitchBase (..)
  , Octave
  , Accidental

  , pitchPosition
  , pitchAccidental
  , pitchAuxLinesTreble
  , pitchClass

  , Interval (..)
  , Directional (..)
  , (<+>)
  , (<->)
  ) where

import Numeric.Natural

data Pitch = Pitch PitchClass Octave
  deriving (Eq)
data PitchClass = PitchClass PitchBase Accidental
  deriving (Eq)
data PitchBase = C | D | E | F | G | A | B
  deriving (Bounded, Enum, Eq, Ord, Show)
type Octave = Int -- c = -1, c' = 0, c'' = 1 ...
type Accidental = Int -- no accidental = 0, b = -1, # = 1

instance Show Pitch where
  show (Pitch pc oct) =
    let oct' = case signum oct of
                 0 -> ""
                 1 -> take oct (repeat '\'')
                 (-1) -> take (abs oct) (repeat ',')
    in show pc ++ oct'

instance Show PitchClass where
  show (PitchClass base acc) =
    let acc' = case signum acc of
                 0 -> ""
                 1 -> take acc (repeat '#')
                 (-1) -> take (abs acc) (repeat 'b')
    in show base ++ acc'

instance Ord Pitch where
  compare (Pitch (PitchClass base1 acc1) oct1) (Pitch (PitchClass base2 acc2) oct2)
    | oct1 > oct2 = GT
    | oct1 < oct2 = LT
    | base1 > base2 = GT
    | base1 < base2 = LT
    | acc1 > acc2 = GT
    | acc1 < acc2 = LT
    | otherwise = EQ

pitchBaseSucc :: PitchBase -> PitchBase
pitchBaseSucc B = C
pitchBaseSucc x = succ x

pitchBasePred :: PitchBase -> PitchBase
pitchBasePred C = B
pitchBasePred x = pred x

pitchAccidental :: Pitch -> Accidental
pitchAccidental (Pitch (PitchClass _ x) _) = x

pitchClass :: Pitch -> PitchClass
pitchClass (Pitch pc _) = pc

-- | Position of the note head in a system centered around the middle C
pitchPosition :: Pitch -> Int
pitchPosition (Pitch (PitchClass base acc) oct) =
  fromEnum base + oct * 7

-- | How many aux lines would a treble clef need for this note.
--   Positive = upper lines, negative = lower lines
pitchAuxLinesTreble :: Pitch -> Int
pitchAuxLinesTreble p
  | pitchPosition p > 0 && pitchPosition p < 12 = 0
  | pitchPosition p <= 0 = (pitchPosition p `div` 2)
  | pitchPosition p >= 12 = (pitchPosition p - 10) `div` 2

data Interval = Interval Natural Int
  deriving (Eq, Show)

intervalPerfect1 = Interval 0 0

instance Semigroup Interval where
  (<>) = addIntervalInterval

instance Monoid Interval where
  mempty = intervalPerfect1

data Directional a = Up a | Down a
  deriving (Eq, Show)

flipDirection :: Directional a -> Directional a
flipDirection (Up x) = Down x
flipDirection (Down x) = Up x

{-
instance Semigroup (Directional Interval) where
  (<>) = addDirIntervalDirInterval
-}

(<+>) = addPitchDirInterval

addIntervalInterval :: Interval -> Interval -> Interval
addIntervalInterval (Interval base1 st1) (Interval base2 st2) =
  Interval (base1 + base2) (st1 + st2)

addPitchDirInterval :: Pitch -> Directional Interval -> Pitch
addPitchDirInterval p (Up i) = addPitchInterval p i
addPitchDirInterval p (Down i) = subPitchInterval p i

addPitchInterval :: Pitch -> Interval -> Pitch
addPitchInterval p (Interval 0 0) = p
addPitchInterval (Pitch (PitchClass base1 acc1) oct1) (Interval 0 st2) =
  Pitch (PitchClass base1 (acc1 + st2)) oct1
addPitchInterval (Pitch (PitchClass base1 acc1) oct1) (Interval base2 st2) =
  let base1' = pitchBaseSucc base1
      acc1' = acc1
      oct1' = case base1 of
                B -> oct1 + 1
                _ -> oct1
      base2' = pred base2
      st2' = case base1 of
               E -> st2 - 1
               B -> st2 - 1
               _ -> st2 - 2

  in addPitchInterval (Pitch (PitchClass base1' acc1') oct1') (Interval base2' st2')

subPitchInterval :: Pitch -> Interval -> Pitch
subPitchInterval p (Interval 0 0) = p
subPitchInterval (Pitch (PitchClass base1 acc1) oct1) (Interval 0 st2) =
  Pitch (PitchClass base1 (acc1 - st2)) oct1
subPitchInterval (Pitch (PitchClass base1 acc1) oct1) (Interval base2 st2) =
  let base1' = pitchBasePred base1
      acc1' = acc1
      oct1' = case base1 of
                C -> oct1 - 1
                _ -> oct1
      base2' = pred base2
      st2' = case base1 of
               F -> st2 - 1
               C -> st2 - 1
               _ -> st2 - 2

  in subPitchInterval (Pitch (PitchClass base1' acc1') oct1') (Interval base2' st2')

(<->) = intervalDistance

intervalDistance :: Pitch -> Pitch -> Directional Interval
intervalDistance p1@(Pitch (PitchClass base1 acc1) oct1) p2@(Pitch (PitchClass base2 acc2) oct2)
  | p1 == p2 = Up intervalPerfect1
  | p1 > p2 = Down (intervalDistance' p2 p1)
  | p1 < p2 = Up (intervalDistance' p1 p2)

intervalDistance' :: Pitch -> Pitch -> Interval
intervalDistance' p1@(Pitch (PitchClass base1 acc1) oct1) p2@(Pitch (PitchClass base2 acc2) oct2)
  | p1 == p2 = intervalPerfect1
  | oct1 < oct2 = Interval 7 12 <> intervalDistance' p1 (Pitch (PitchClass base2 acc2) (oct2 - 1))
  | base1 < base2 = let i = case base2 of
                              F -> Interval 1 1
                              C -> Interval 1 1
                              _ -> Interval 1 2
                    in i <> intervalDistance' p1 (Pitch (PitchClass (pitchBasePred base2) acc2) oct2)
  | acc1 /= acc2 = Interval 0 (acc2 - acc1)
