{-# OPTIONS_GHC -fno-warn-orphans #-}

module TimeSpec where

import Effects.Time
import Polysemy
import Test.Hspec
import Test.Hspec.QuickCheck
import Utils ()

spec :: Spec
spec =
  describe "runFixedTime" $
    prop "always returns the given time" $ \fixedTime ->
      run (runFixedTime fixedTime getTime)
        `shouldBe` fixedTime
