module Linguisticks.Statistics where

import qualified Linguisticks.Util as U

import qualified Data.Vector.Unboxed as V
import qualified Statistics.Sample as S
import qualified Statistics.Distribution.StudentT as StT
import qualified Statistics.Distribution.Normal as N
import qualified Statistics.Distribution as D
import Data.Either ( rights )

buildSyllablesSample :: (U.Syllables -> Double) -> [U.ParsedWord U.Syllables] -> S.Sample
buildSyllablesSample f = V.fromList . map f . rights . map U.wordSyllables
{- More efficient single-pass implementation:
buildSample f ws = let listResults []     = []
                       listResults (w:ws) = let wordSylls = U.wordSyllables w
                                            in either (const $ listResults ws)
                                                      (\s -> (f s):(listResults ws))
                                                      wordSylls
                   in V.fromList (listResults ws)
-}

sampleLengths :: [U.ParsedWord U.Syllables] -> S.Sample
sampleLengths = buildSyllablesSample (fromIntegral . length . U.syllsToList)

notPhonemes :: [Char]
notPhonemes = ['̩', 'ː', U.primaryStress, U.secondaryStress]

samplePhonemeLengths :: [U.Word a] -> S.Sample
samplePhonemeLengths = let phonemeLength = length . filter (not . (`elem` notPhonemes)) . U.wordTranscription
                       in V.fromList . map (fromIntegral . phonemeLength)

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
                                 in (D.complCumulative myStT absTValue, flip compare 0 tValue)

xBarDistribution :: S.Sample -> N.NormalDistribution
xBarDistribution sample = let (xbar, var) = S.meanVariance sample
                              n = fromIntegral $ V.length sample
                          in N.normalDistr xbar (sqrt (var/n))

xBarConfidenceInterval :: Double -> S.Sample -> (Double, Double)
xBarConfidenceInterval confidence sample = let tailSize = (1-confidence)/2
                                               distribution = xBarDistribution sample
                                               lower = D.quantile distribution tailSize
                                               upper = D.quantile distribution (1-tailSize)
                                           in (lower, upper)
