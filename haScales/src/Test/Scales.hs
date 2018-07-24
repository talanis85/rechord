module Test.Scales where

import Data.Music.Scales
import Test.QuickCheck

instance Arbitrary DegreeBase where
    arbitrary = elements [I, II, III, IV, V, VI, VII]

instance Arbitrary Accidental where
    arbitrary = do
        n <- choose (-3, 3)
        return $ Accidental n

instance Arbitrary Degree where
    arbitrary = do
        base <- arbitrary
        acc <- arbitrary
        return $ Degree base acc

instance Arbitrary Scale where
    arbitrary = return $ ionianScale -- TODO!
