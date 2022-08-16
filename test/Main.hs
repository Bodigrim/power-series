{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main
  ( main
  ) where

import Data.Mod.Word
import qualified Data.Poly.Semiring as Poly
import Data.Semiring (Semiring)
import Data.Series.Power
import qualified Data.Vector.Generic as G
import GHC.TypeNats (KnownNat)
import Test.Tasty
import Test.Tasty.QuickCheck

instance KnownNat m => Arbitrary (Mod m) where
  arbitrary = oneof [arbitraryBoundedEnum, fromInteger <$> arbitrary]
  shrink = map fromInteger . shrink . toInteger . unMod

instance (Eq a, Semiring a, Arbitrary a, G.Vector v a) => Arbitrary (Poly.Poly v a) where
  arbitrary = Poly.toPoly . G.fromList <$> arbitrary
  shrink = fmap (Poly.toPoly . G.fromList) . shrink . G.toList . Poly.unPoly

main :: IO ()
main = defaultMain $ testGroup "All"
  [ testProperty "recip" $
    \p -> Poly.eval p 0 /= 0 ==> let s = fromPoly p in
      toPoly 100 (s * recip s :: USeries (Mod 11)) === 1
  ]
