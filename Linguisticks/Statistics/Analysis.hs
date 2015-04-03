module Linguisticks.Statistics.Analysis where

import qualified Linguisticks.Util as U
import qualified Linguisticks.Statistics.Analysis.StudentT as StT
import qualified Linguisticks.Statistics.Analysis.Mean as Mean
import qualified Linguisticks.Statistics.Processing as Pr

import qualified Statistics.Sample as S
import qualified Statistics.Distribution as D

studentTPValue = StT.studentTPValue
studentTValue = StT.studentTValue
meanConfidenceInterval = Mean.meanConfidenceInterval

twoSampleTest :: ([U.Word b] -> [U.Word b] -> a) -> [(String, [U.Word b])] -> [(String, String, a)]
twoSampleTest test samples = do samplePair <- U.choose 2 samples
                                let (id1, ps1) = samplePair !! 0
                                    (id2, ps2) = samplePair !! 1
                                return (id1, id2, test ps1 ps2)

tTest :: ([U.Word a] -> S.Sample) -> [(String, [U.Word a])] -> [(String, String, Double, Ordering)]
tTest pr = let flatten (a, b, (c, d)) = (a, b, c, d)
           in map flatten . twoSampleTest (Pr.twoSampleProcess pr studentTPValue)

{-
class SampleTest t where
  getPValue :: t -> S.Sample -> Double
  getProcessor :: t -> (a -> S.Sample)
  processAndGetPValue :: t -> a -> Double
  processAndGetPValue t = (getPValue t) . (getProcessor t)

data MeanStudentT = MeanStudentT { mSTProcessor :: [a] -> S.Sample }

instance SampleTest MeanStudentT where
  getPValue _ = studentTPValue
-}

{-
class HypothesisTest t where
  getDistr :: D.Distribution g => t -> g
  hypPValue :: t -> Double -> Double
  hypPValue t v = let distr :: D.Distribution f => f
                      distr = getDistr t
                  in D.complCumulative distr (abs v)

class HypothesisTest t => SampleTest t where
-}
