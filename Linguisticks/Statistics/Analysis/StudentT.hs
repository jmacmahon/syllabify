module Linguisticks.Statistics.Analysis.StudentT where

import Debug.Trace ( trace )

import qualified Linguisticks.Util as U

import qualified Data.Vector.Unboxed as V
import qualified Statistics.Sample as S
import qualified Statistics.Distribution.StudentT as StT
import qualified Statistics.Distribution as D

welchV :: S.Sample -> S.Sample -> Double
welchV sample1 sample2 = let (xbar1, var1) = S.meanVariance sample1
                             (xbar2, var2) = S.meanVariance sample2
                             ratio1 = var1 / (fromIntegral $ V.length sample1)
                             ratio2 = var2 / (fromIntegral $ V.length sample2)
                             v1 = fromIntegral $ (V.length sample1) - 1
                             v2 = fromIntegral $ (V.length sample2) - 1
                             numerator = (ratio1 + ratio2) ^ 2
                             denominator = ((ratio1 ^ 2)/v1) + ((ratio2 ^ 2)/v2)
                         in numerator/denominator

studentTValue :: S.Sample -> S.Sample -> Double
studentTValue sample1 sample2 = let (xbar1, var1) = S.meanVariance sample1
                                    (xbar2, var2) = S.meanVariance sample2
                                    n1 = fromIntegral $ V.length sample1
                                    n2 = fromIntegral $ V.length sample2
                                in (xbar1 - xbar2)/(sqrt (var1/n1 + var2/n2))

studentTPValue :: S.Sample -> S.Sample -> (Double, Ordering)
studentTPValue sample1 sample2 = let tValue = studentTValue sample1 sample2
                                     absTValue = abs tValue
                                     v = welchV sample1 sample2
                                     myStT = StT.studentT v
                                 in if isNaN tValue
                                    then (0.5, EQ)
                                    else(D.complCumulative myStT absTValue, flip compare 0 tValue)
