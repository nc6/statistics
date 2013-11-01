{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Statistics.Distribution.BetaBinomial
-- Copyright   :  (C) 2013 Nicholas Clarke,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Nicholas Clarke <impredicative@topos.org.uk>
-- Stability   :  provisional
-- Portability :  DeriveDataTypeable
--
----------------------------------------------------------------------------
module Statistics.Distribution.BetaBinomial 
  (
      BetaBinomialDistribution
    , bbdAlpha
    , bbdBeta
    , bbdNumberOfTrials
  ) where
  import Numeric.SpecFunctions (choose, incompleteBeta)
  import qualified Statistics.Distribution as D

  data BetaBinomialDistribution = BetaBinomialDistribution {
      bbdAlpha :: {-# UNPACK #-} !Double
    , bbdBeta :: {-# UNPACK #-} !Double
    , bbdNumberOfTrials :: {-# UNPACK #-} !Int
  }

  -- | Create beta binomial distribution. Both shape parameters must be positive.
  betaBinomialDistr :: Double             -- ^ Shape parameter alpha
            -> Double             -- ^ Shape parameter beta
            -> Int                -- ^ Number of trials
            -> BetaBinomialDistribution
  betaBinomialDistr a b
    | a > 0 && b > 0 = improperBetaBinomialDistr a b
    | otherwise      =
        error $  "Statistics.Distribution.BetaBinomial.betaBinomialDistr: "
              ++ "shape parameters must be positive. Got a = "
              ++ show a
              ++ " b = "
              ++ show b
  {-# INLINE betaBinomialDistr #-}

  improperBetaBinomialDistr :: Double -- ^ Shape parameter alpha
                            -> Double -- ^ Shape parameter beta
                            -> Int -- ^ Number of trials
                            -> BetaBinomialDistribution
  improperBetaBinomialDistr = BetaBinomialDistribution
  {-# INLINE improperBetaBinomialDistr #-}

  instance D.DiscreteDistr BetaBinomialDistribution where
    probability d@(BetaBinomialDistribution a b n) k = (n `choose` k) * b1 / b2 where
      b1 = incompleteBeta (x + a) (n' - x + b) 1
      b2 = incompleteBeta a b 1
      x = fromIntegral k
      n' = fromIntegral n
    {-# INLINE probability #-}

  instance D.Distribution BetaBinomialDistribution where
    cumulative d@(BetaBinomialDistribution a b n) x
      | isNaN x      = error "Statistics.Distribution.BetaBinomialDistribution.cumulative: NaN argument"
      | isInfinite x = if x > 0 then 1 else 0
      | x' <= 0 = 0
      | x' >= n = 1
      | otherwise = D.sumProbabilities d 0 x'
      where x' = floor x

  instance D.Mean BetaBinomialDistribution where
    mean (BetaBinomialDistribution a b n) = ((fromIntegral n)*a) / (a + b)
    {-# INLINE mean #-}

  instance D.MaybeMean BetaBinomialDistribution where
    maybeMean = Just . D.mean
    {-# INLINE maybeMean #-}

  instance D.Variance BetaBinomialDistribution where
    variance (BetaBinomialDistribution a b n) = let n' = fromIntegral n in
      (n'*a*b)*(a+b+n')/(((a+b)^2) * (a + b + 1))

  instance D.MaybeVariance BetaBinomialDistribution where
    maybeVariance = Just . D.variance
