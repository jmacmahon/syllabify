module Linguisticks.Statistics.Analysis where

import qualified Linguisticks.Statistics.Analysis.StudentT as StT
import qualified Linguisticks.Statistics.Analysis.Mean as Mean

import qualified Statistics.Sample as S
import qualified Statistics.Distribution as D

studentTPValue = StT.studentTPValue
studentTValue = StT.studentTValue
meanConfidenceInterval = Mean.meanConfidenceInterval

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
