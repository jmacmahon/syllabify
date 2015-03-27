module Linguisticks where

import qualified Linguisticks.Util as U
import qualified Linguisticks.Parser as P
import qualified Linguisticks.CSV as C
import qualified Linguisticks.Statistics as S

import Text.Parsec.Error ( ParseError )

type IPS = IO [U.ParsedWord U.Syllables]

parsed_elicited :: IPS
parsed_elicited = C.readParsedWords "data/elicited_parsed.csv"

parsed_real :: IPS
parsed_real = C.readParsedWords "data/real_words_1_parsed.csv"

parsed_carroll_lear :: IPS
parsed_carroll_lear = C.readParsedWords "data/carroll+lear_parsed.csv"
