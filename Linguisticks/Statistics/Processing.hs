module Linguisticks.Statistics.Processing where

import qualified Linguisticks.Util as U

import qualified Data.Vector.Unboxed as V
import qualified Statistics.Sample as S
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

consonants = "pbtdkgfvθðszʃʒʧʤmnlrwjhŋɫ"
vowels = "ɛɪaɔəʊiueɑʌoːɜɒ"

-- Several passes, but premature optimisation is the root of all evil
consonantLength = fromIntegral . length . filter (`elem` consonants)
vowelLength = fromIntegral . length . filter (`elem` vowels)
                              
cvRatio :: U.Word a -> Double
cvRatio w = (consonantLength $ U.wordTranscription w) / (vowelLength $ U.wordTranscription w)

sampleCVRatio :: [U.Word a] -> S.Sample
sampleCVRatio = V.fromList . map cvRatio

consonantRatio :: Char -> U.Word a -> Double
consonantRatio c w = let consonantLength = length $ filter (== c) $ U.wordTranscription w
                         wLength = length $ U.wordTranscription w
                     in (fromIntegral consonantLength) / (fromIntegral wLength)

sampleConsonantRatio :: Char -> [U.Word a] -> S.Sample
sampleConsonantRatio c = V.fromList . map (consonantRatio c)

--consonantCount :: String -> Double
--consonantCount = 

sampleOnsetConsonants :: [U.ParsedWord U.Syllables] -> S.Sample
sampleOnsetConsonants = let gatherConsonants :: [U.Syllables] -> [Double]
                            gatherConsonants []     = []
                            gatherConsonants (s:ss) = let sylls = U.syllsToList s
                                                          onsets = map (\(o, _, _, _) -> o) sylls
                                                          consonantCounts = map consonantLength onsets
                                                      in consonantCounts ++ (gatherConsonants ss)
                        in V.fromList . gatherConsonants . rights . map U.wordSyllables

twoSampleProcess :: (a -> S.Sample) -> (S.Sample -> S.Sample -> b) -> a -> a -> b
twoSampleProcess processor test a1 a2 = test (processor a1) (processor a2)
