module Linguisticks.Statistics.Analysis.Mean where

import qualified Linguisticks.Util as U

import qualified Data.Vector.Unboxed as V
import qualified Statistics.Sample as S
import qualified Statistics.Distribution.Normal as N
import qualified Statistics.Distribution as D

meanDistribution :: S.Sample -> Either String N.NormalDistribution
meanDistribution sample = let (xbar, var) = S.meanVariance sample
                              n = fromIntegral $ V.length sample
                          in if var == 0
                             then Left "0 variance"
                             else Right $ N.normalDistr xbar (sqrt (var/n))

meanConfidenceInterval :: Double -> S.Sample -> Either String (Double, Double)
meanConfidenceInterval confidence sample = let tailSize = (1-confidence)/2
                                           in do distribution <- meanDistribution sample
                                                 let lower = D.quantile distribution tailSize
                                                     upper = D.quantile distribution (1-tailSize)
                                                 return (lower, upper)
