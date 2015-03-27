module Linguisticks.CSV where

import Linguisticks.Util ( Syllables, readParserMaybe, zeroState, maybeP )

import Text.CSV ( CSV, parseCSVFromFile, Record )
import Data.Maybe ( catMaybes )
import Text.ParserCombinators.Parsec ( ParseError, runParser )

-- Reading the CSV
data Word a = Word { wordSpelling :: String, wordTranscription :: String, wordSyllables :: a }
            deriving (Show, Eq)

recordToRaw :: Record -> Maybe (Word ())
recordToRaw rec_ | length rec_ < 2 = Nothing
                 | otherwise       = Just $ Word (rec_ !! 0) (rec_ !! 1) ()

readRawWords :: FilePath -> IO [Word ()]
readRawWords fp = let rawWords :: CSV -> [Word ()]
                      rawWords = catMaybes . map recordToRaw
                  in do errorOrCSV <- parseCSVFromFile fp
                        either (fail . show) (return . rawWords) errorOrCSV

readAndParse :: (String -> Either ParseError Syllables) -> FilePath -> IO [Word (Either ParseError Syllables)]
readAndParse p fp = let processWord w = let sylls = p $ wordTranscription w
                                        in w { wordSyllables = sylls }
                    in do raw <- readRawWords fp
                          return $ map processWord raw

recordToParsed :: Record -> Maybe (Word (Either ParseError Syllables))
recordToParsed rec_ | length rec_ < 3 = Nothing
                    | otherwise       = let spelling = rec_ !! 0
                                            transcription = rec_ !! 1
                                            rawSyllables = rec_ !! 2
                                            parsedSyllables = runParser (maybeP readParserMaybe) zeroState "" rawSyllables
                                        in Just $ Word spelling transcription parsedSyllables

readParsedWords :: FilePath -> IO [Word (Either ParseError Syllables)]
readParsedWords fp = let parsedWords :: CSV -> [Word (Either ParseError Syllables)]
                         parsedWords = catMaybes . map recordToParsed
                     in do errorOrCSV <- parseCSVFromFile fp
                           either (fail . show) (return . parsedWords) errorOrCSV

-- Writing the CSV
rowFormat :: Word (Either a Syllables) -> String
rowFormat w = let sylls = wordSyllables w
                  syllFormatted = w { wordSyllables = either (const "ERROR") show sylls }
              in (wordSpelling syllFormatted)
                 ++ "," ++ (wordTranscription syllFormatted)
                 ++ "," ++ (wordSyllables syllFormatted)
