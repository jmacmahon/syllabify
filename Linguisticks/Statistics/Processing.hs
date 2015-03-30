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
cvRatio :: U.Word a -> Double
cvRatio w = let consonantLength = length $ filter (`elem` consonants) $ U.wordTranscription w
                vowelLength = length $ filter (`elem` vowels) $ U.wordTranscription w
            in fromIntegral consonantLength / fromIntegral vowelLength

sampleCVRatio :: [U.Word a] -> S.Sample
sampleCVRatio = V.fromList . map cvRatio
