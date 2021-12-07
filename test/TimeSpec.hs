{-# OPTIONS_GHC -fno-warn-orphans #-}

module TimeSpec where

import Data.Time
import Data.Time.Clock.POSIX
import Effects.Time
import Polysemy
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

instance Arbitrary UTCTime where
  arbitrary = posixSecondsToUTCTime . fromInteger <$> arbitrary

spec :: Spec
spec =
  describe "runFixedTime" $
    prop "always returns the given time" $ \fixedTime ->
      run (runFixedTime fixedTime getTime)
        `shouldBe` fixedTime
