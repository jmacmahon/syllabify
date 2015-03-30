module Linguisticks.Statistics.Analysis.Mean where

import qualified Linguisticks.Util as U

import qualified Data.Vector.Unboxed as V
import qualified Statistics.Sample as S
import qualified Statistics.Distribution.Normal as N
import qualified Statistics.Distribution as D

meanDistribution :: S.Sample -> N.NormalDistribution
meanDistribution sample = let (xbar, var) = S.meanVariance sample
                              n = fromIntegral $ V.length sample
                          in N.normalDistr xbar (sqrt (var/n))

meanConfidenceInterval :: Double -> S.Sample -> (Double, Double)
meanConfidenceInterval confidence sample = let tailSize = (1-confidence)/2
                                               distribution = meanDistribution sample
                                               lower = D.quantile distribution tailSize
                                               upper = D.quantile distribution (1-tailSize)
                                           in (lower, upper)
