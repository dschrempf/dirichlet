-- |
-- Module      :  Statistics.Distribution.Dirichlet
-- Description :  Multivariate Dirichlet distribution
-- Copyright   :  (c) Dominik Schrempf, 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Oct 20 10:10:39 2020.
module Statistics.Distribution.Dirichlet
  ( DirichletDistribution (ddGetParameters),
    dirichletDistribution,
    dirichletDensity,
    dirichletSample,
  )
where

import Control.Monad.Primitive
import qualified Data.Vector.Unboxed as V
import Numeric.Log
import Numeric.SpecFunctions
import System.Random.MWC
import System.Random.MWC.Distributions

-- TODO: Also provide a symmetric version.

-- | The Dirichlet distribution is identified by a vector of parameter values.
data DirichletDistribution = DirichletDistribution
  { ddGetParameters :: V.Vector Double,
    _getNormConst :: Log Double,
    _getDimension :: Int
  }

-- Check if vector is strictly positive.
isNegativeOrZero :: V.Vector Double -> Bool
isNegativeOrZero = V.any (<= 0)

-- Inverse multivariate beta function. Does not check if parameters are valid!
invBeta :: V.Vector Double -> Log Double
invBeta v = Exp $ logDenominator - logNominator
  where
    logNominator = V.sum $ V.map logGamma v
    logDenominator = logGamma (V.sum v)

-- | Create a Dirichlet distribution from the given parameter vector.
--
-- Return Left if:
-- - The parameter vector is empty.
-- - One or more parameters are negative or zero.
dirichletDistribution :: V.Vector Double -> Either String DirichletDistribution
dirichletDistribution v
  | V.null v =
    Left "dirichletDistribution: Parameter vector is empty."
  | isNegativeOrZero v =
    Left "dirichletDistribution: One or more parameters are negative or zero."
  | otherwise = Right $ DirichletDistribution v (invBeta v) (V.length v)

-- Tolerance.
eps :: Double
eps = 1e-14

-- Check if vector is normalized with tolerance 'eps'.
isNormalized :: V.Vector Double -> Bool
isNormalized v
  | abs (V.sum v - 1.0) > eps = False
  | otherwise = True

-- Check if vector has negative elements.
isNegative :: V.Vector Double -> Bool
isNegative = V.any (< 0)

-- | Density of the Dirichlet distribution evaluated at a given value vector.
--
-- Return 0 if:
-- - The value vector has a different length than the parameter vector.
-- - The value vector is empty.
-- - The value vector has negative elements.
-- - The value vector does not sum to 1.0 (with tolerance @eps = 1e-14@).
dirichletDensity :: DirichletDistribution -> V.Vector Double -> Log Double
dirichletDensity (DirichletDistribution as n k) xs
  | k /= V.length xs = 0
  | V.null xs = 0
  | isNegative xs = 0
  | not (isNormalized xs) = 0
  | otherwise = n * Exp logXsPow
  where
    logXsPow = V.sum $ V.zipWith (\a x -> log $ x ** (a - 1.0)) as xs

-- | Sample a value vector from the Dirichlet distribution.
dirichletSample :: PrimMonad m => DirichletDistribution -> Gen (PrimState m) -> m (V.Vector Double)
dirichletSample (DirichletDistribution as _ _) g = do
  ys <- V.mapM (\a -> gamma a 1.0 g) as
  let s = V.sum ys
  return $ V.map (/ s) ys
