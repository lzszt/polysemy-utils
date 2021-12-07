{-# OPTIONS_GHC -fno-warn-orphans #-}

module Utils where

import Effects.Cache
import Test.QuickCheck
import Test.QuickCheck.Instances ()

instance Arbitrary CacheStrategy where
  arbitrary = oneof [pure Indefinitly, CacheFor . getPositive <$> arbitrary]
