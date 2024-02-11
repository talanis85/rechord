module Data.Chord
  ( Chord (..)
  , ChordType (..)

  , mapChordPitchClass
  , mapChordPitch
  ) where

import Data.Pitch

data Chord = Chord PitchClass (Maybe PitchClass) ChordType
data ChordType = ChordType String String

instance Show Chord where
  show (Chord pc Nothing (ChordType t1 t2)) = show pc ++ t1 ++ t2
  show (Chord pc (Just slash) (ChordType t1 t2)) = show pc ++ t1 ++ t2 ++ "/" ++ show slash

mapChordPitchClass :: (PitchClass -> PitchClass) -> Chord -> Chord
mapChordPitchClass f (Chord pc slash t) = Chord (f pc) (fmap f slash) t

mapChordPitch :: (Pitch -> Pitch) -> Chord -> Chord
mapChordPitch f (Chord pc slash t) =
  let Pitch pc' _ = f (Pitch pc 0)
      slash' = fmap (pitchClass . f . flip Pitch 0) slash
  in Chord pc' slash' t
