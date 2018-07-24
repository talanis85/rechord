module Test.Chords where

import Data.Music.Chords
import Test.QuickCheck

import Test.Scales

instance Arbitrary Chord where
    arbitrary = do
        noteCount <- choose (1,12)
        scale <- arbitrary
        degrees <- vectorOf noteCount arbitrary
        slash <- arbitrary
        return $ mkChord scale degrees </> slash

-----------------------------------------------------------------------------
-- Properties for chords
-----------------------------------------------------------------------------

-- 
