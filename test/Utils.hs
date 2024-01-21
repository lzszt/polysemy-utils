{-# OPTIONS_GHC -fno-warn-orphans #-}

module Utils where

import Effects.Cache
import Test.QuickCheck
import Test.QuickCheck.Instances ()

instance Arbitrary (CacheStrategy Indef) where
  arbitrary = pure Indefinitly

instance Arbitrary (CacheStrategy Finite) where
  arbitrary = CacheFor . getPositive <$> arbitrary