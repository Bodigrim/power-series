{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Series.Power
  ( Series
  , VSeries
  , USeries
  , fromPoly
  , toPoly
  , deriv
  , integral

  , expSeriesAt
  , log1pSeries
  , sqrt1pSeries
  ) where

import qualified Data.Chimera as Ch
import Data.Foldable
import qualified Data.Poly.Semiring as Poly
import qualified Data.Semiring as S
import qualified Data.Vector.Generic as G
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified GHC.Exts as Exts

newtype Series v a = Series { unSeries :: Ch.Chimera v a }

type VSeries = Series V.Vector

type USeries = Series U.Vector

instance (Eq a, S.Semiring a, G.Vector v a) => Exts.IsList (Series v a) where
  type Item (Series v a) = a
  fromList = Series . Ch.fromListWithDef S.zero
  fromListN = (fromPoly .) . Exts.fromListN
  toList = Ch.toList . unSeries

fromPoly :: (G.Vector v a, S.Semiring a) => Poly.Poly v a -> Series v a
fromPoly = Series . Ch.fromVectorWithDef S.zero . Poly.unPoly

toPoly :: (Eq a, S.Semiring a, G.Vector v a) => Int -> Series v a -> Poly.Poly v a
toPoly len = Poly.toPoly . G.concat . Ch.sliceSubvectors 0 len . unSeries

instance (Num a, G.Vector v a) => Num (Series v a) where
  negate (Series xs) = Series $ Ch.mapSubvectors (G.map negate) xs
  Series xs + Series ys = Series $ Ch.zipWithSubvectors (G.zipWith (+)) xs ys
  Series xs - Series ys = Series $ Ch.zipWithSubvectors (G.zipWith (-)) xs ys
  abs = id
  signum = const 1
  fromInteger i = Series $ Ch.fromListWithDef 0 [fromInteger i]

  Series xs * Series ys = Series $ Ch.tabulate $
    \n -> foldl' (+) 0 $ map (\i -> Ch.index xs i * Ch.index ys (n - i)) [0..n]

deriv :: (G.Vector v a, Num a) => Series v a -> Series v a
deriv (Series xs) = Series $ Ch.tabulate $ \n -> fromIntegral (n + 1) * Ch.index xs (n + 1)

integral :: (G.Vector v a, Fractional a) => Series v a -> Series v a
integral (Series xs) = Series $ Ch.tabulate $ \n -> if n == 0 then 0 else Ch.index xs (n - 1) / fromIntegral n

instance forall v a. (Eq a, Fractional a, G.Vector v a) => Fractional (Series v a) where
  fromRational r = Series $ Ch.fromListWithDef 0 [fromRational r]

  -- https://en.wikipedia.org/wiki/Formal_power_series#Multiplicative_inverse
  recip (Series as) = Series $ Ch.tabulateFix $ \rec ->
    \n -> if n == 0
      then recip (Ch.index as 0)
      else - (foldl' (+) 0 $ map (\i -> Ch.index as i * rec (n - i)) [1..n]) / Ch.index as 0

expSeriesAt0 :: (G.Vector v a, Fractional a) => Series v a
expSeriesAt0 = Chimera $ Ch.iterateWithIndex (\ix acc -> acc / fromIntegral ix) 1

expSeriesAt :: (G.Vector v a, Floating a) => a -> Series v a
expSeriesAt x0 = Series $ Ch.mapSubvectors (G.map (* exp x0)) $ unSeries $ expSeriesAt0

logSeriesAt1 :: (G.Vector v a, Fractional a) => Series v a
logSeriesAt1 = Series $ Ch.tabulate $
  \n -> if n == 0 then 0 else (if odd n then recip (fromIntegral n) else - recip (fromIntegral n))

-- https://en.wikipedia.org/wiki/Taylor_series#Examples
logSeriesAt :: (G.Vector v a, Floating a) => a -> Series v a
logSeriesAt x0 = Series $ Ch.iterateWithIndex
  (\i acc -> if i == 1 then recip x0 else negate acc / x0)
  (log x0)

-- https://en.wikipedia.org/wiki/Fa%C3%A0_di_Bruno%27s_formula#Formal_power_series_version
compose :: (a -> Series v a) -> Series v a -> Series v a
compose ff g = fg
  where
    g0 = Ch.index g' 0
    f = ff g0
    fg = Series $ Ch.tabulate mkC
    mkC n = undefined

generatePnk :: Int -> Int -> [[Int]]
generatePnk n k = undefined

multinomial :: [Int] -> Integer
multinomial xs = foldl'
  (\acc x -> acc `quot` Ch.index factorials x)
  (Ch.index factorials (sum xs)) xs

factorials :: Ch.VChimera Integer
factorials = Ch.iterateWithIndex (\i acc -> fromIntegral i * acc) 1
