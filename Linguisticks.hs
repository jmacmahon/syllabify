module Linguisticks where

import qualified Linguisticks.Util as U
import qualified Linguisticks.Parser as Par
import qualified Linguisticks.CSV as C
import qualified Linguisticks.Statistics.Analysis as An
import qualified Linguisticks.Statistics.Processing as Pr

import Text.Parsec.Error ( ParseError )

type PS = [U.ParsedWord U.Syllables]

parsed_elicited = C.readParsedWords "data/elicited_parsed.csv"
parsed_real = C.readParsedWords "data/real_words_1_parsed.csv"
parsed_carroll_lear = C.readParsedWords "data/carroll+lear_parsed.csv"
parsed_hitchhikers = C.readParsedWords "data/hitchhikers_parsed.csv"
parsed_dahl_rowling = C.readParsedWords "data/dahl_rowling_parsed.csv"
parsed_gullivers = C.readParsedWords "data/gullivers_parsed.csv"

parsed_all :: IO [(String, PS)]
parsed_all = U.sequenceSecond $ [ ("elicited", parsed_elicited)
                                , ("real", parsed_real)
                                , ("carroll_lear", parsed_carroll_lear)
                                , ("hitchhikers", parsed_hitchhikers)
                                , ("dahl_rowling", parsed_dahl_rowling)
                                , ("gullivers", parsed_gullivers)
                                ]
