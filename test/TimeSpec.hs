{-# OPTIONS_GHC -fno-warn-orphans #-}

module TimeSpec where

import Effects.Time
import Polysemy
import Test.Hspec
import Test.Hspec.QuickCheck
import Utils ()

spec :: Spec
spec =
  describe "runFixedTime" $ do
    prop "always returns the given time" $ \fixedTime timeZone ->
      run (runFixedTime (fixedTime, timeZone) getTime)
        `shouldBe` fixedTime
    prop "always returns the given time zone" $ \fixedTime timeZone ->
      run (runFixedTime (fixedTime, timeZone) getCurrentTimeZone)
        `shouldBe` timeZone
