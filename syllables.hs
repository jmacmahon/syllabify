module Syllables ( parse
                 , trousers
                 , improvements
                 ) where

import Text.ParserCombinators.Parsec
import Data.Maybe ( isJust, fromMaybe, fromJust, isNothing )
import Debug.Trace ( trace )

-- Reading the CSV:
data Word a = Word { wordSpelling :: String, wordTranscription :: String, wordSyllables :: a }
            deriving (Show, Eq)

instance Functor Word where
  fmap f w = w { wordSyllables = f (wordSyllables w) }

csvParser :: P [Word String]
csvParser = many $ do w <- csvWord
                      char '\n'
                      return w

csvWord :: P (Word String)
csvWord = do spelling <- many $ noneOf ",\n"
             char ','
             transcription <- many $ noneOf ",\n"
             let strippedTrans = strip transcription
             return $ Word (strip spelling) strippedTrans strippedTrans

strip = lstrip . rstrip
lstrip = dropWhile (`elem` " \t")
rstrip = reverse . lstrip . reverse

fromRight :: Either a b -> b
fromRight (Right b) = b

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

-- Some example transcriptions
trousers = "trˈaʊzəz"
improvements = "ɪmprˈu:vmənts"
glimpsed = "glˈɪmpst"
table = "tɛɪbɫ̩"
rhythm = "rɪðm̩"

-- STRESS MARKER
stressMarker = 'ˈ'

-- ONSETS SECTION
singleOnsets = ["p", "b", "t", "d", "k", "g", "f", "v", "θ", "ð", "s", "z", "ʃ", "ʒ", "ʧ", "ʤ", "m", "n", "l", "r", "w", "j", "h"]
doubleOnsets = ["pl", "pr", "pj", "bl", "br", "bj", "tr", "tj", "tw", "dr", "dj", "dw", "kl", "kr", "kj", "kw", "gl", "gr", "gj", "gw", "mj", "nj", "fl", "fr", "fj", "vj", "θr", "θj", "θw", "sl", "sj", "sw", "sp", "st", "sk", "sm", "sn", "ʃr", "hj", "lj", "zj"]
tripleOnsets = ["spl", "spr", "spj", "str", "stj", "skl", "skr", "skj", "skw"]
validOnsets :: [[Char]]
validOnsets = tripleOnsets ++ doubleOnsets ++ singleOnsets ++ [""]

-- NUCLEI SECTION
syllabicConsonants = ["ɫ̩", "n̩", "m̩", "r̩"]
dipthongs = ["ɛɪ", "aɪ", "ɔɪ", "əʊ", "aʊ", "iə", "ɛə", "uə", "eɪ", "ɑɪ", "ʌʊ", "oʊ", "ɛʊ", "eʊ", "eə", "ʊə", "ɪə"]
longVowels = ["iː", "ɑː", "ɔː", "uː", "ɜː", "eː", "oː", "ɪː"]
shortVowels = ["ɪ", "ɛ", "a", "ʌ", "ɒ", "ʊ", "ə", "i", "e", "o", "u", "ɔ"]
validNuclei :: [[Char]]
validNuclei = syllabicConsonants ++ dipthongs ++ longVowels ++ shortVowels
validNucleiParsers :: [P String]
validNucleiParsers = map (try . string) validNuclei

-- CODAS SECTION
singleCodas = ["p", "b", "t", "d", "k", "g", "f", "v", "θ", "ð", "s", "z", "ʃ", "ʒ", "ʧ", "ʤ", "m", "n", "ŋ", "l", "r"]
doubleCodas = ["pt", "pθ", "ps", "tθ", "ts", "kt", "ks", "kθ", "bd", "bz", "dz", "gd", "gz", "ʧt", "ʤd", "mp", "md", "mf", "mθ", "mz", "nt", "nd", "nʧ", "nʤ", "nθ", "ns", "nz", "ŋd", "ŋk", "ŋz", "ŋθ", "lp", "lt", "lk", "lb", "ld", "lʧ", "lʤ", "lm", "ln", "lf", "lv", "lθ", "ls", "lz", "lʃ", "ft", "fθ", "fs", "vd", "vz", "θt", "θs", "ðd", "ðz", "sp", "st", "sk", "zd", "ʃt", "ʒd", "rp", "rb", "rt", "rd", "rt", "rʧ", "rʤ", "rk", "rg", "rm", "rn", "rl"]
tripleCodas = ["pts", "pst", "pθs", "tst", "tθs", "dst", "kts", "kst", "ksθ", "kθs", "mpt", "mps", "mfs", "ntθ", "nts", "ndz", "nʧt", "nʤd", "nθs", "nst", "nzd", "ŋkt", "ŋkθ", "ŋks", "lpt", "lps", "lts", "lkt", "lks", "lbz", "ldz", "lʧt", "lʤd", "lmd", "lmz", "lnz", "lfs", "lfθ", "lvd", "lvz", "lθs", "lst", "fts", "fθs", "spt", "sps", "sts", "skt", "sks", "rmθ", "rpt", "rps", "rts", "rst", "rkt"]
quadCodas = ["mpts", "mpst", "lkts", "lpts", "lfθs", "ksts", "ksθs", "ntθs"]
validCodas :: [[Char]]
validCodas = [""] ++ singleCodas ++ doubleCodas ++ tripleCodas ++ quadCodas

-- ADT for a Syllable.
data Stress = NoStress | Secondary | Primary deriving (Eq, Show, Ord, Enum)
data Syllables = Syllables { syllOnset :: String, syllNucleus :: String, syllCoda :: String, syllStress :: Stress, syllNext :: Maybe Syllables }
                 deriving (Eq)

syllsToList :: Syllables -> [(String, String, String, Stress)]
syllsToList s@(Syllables ons nuc cod stress next) | isNothing next = [(ons, nuc, cod, stress)]
                                                  | otherwise      = (ons, nuc, cod, stress):(syllsToList $ fromJust next)

-- Convenience functions for the parser and representations
parseSyllables :: String -> Either ParseError Syllables
parseSyllables = parse syllables ""

nullSign = "∅"
nullReplace s = if null s then nullSign else s

showFirstSyllable (Syllables ons nuc cod stress _) = let stressMarker = if stress == Primary
                                                                        then "ˈ"
                                                                        else if stress == Secondary
                                                                             then "ˌ"
                                                                             else ""
                                                     in stressMarker
                                                        ++ "(" ++ nullReplace ons
                                                        ++ " " ++ nullReplace nuc
                                                        ++ " " ++ nullReplace cod
                                                        ++ ")"
instance Show Syllables where
  show s@(Syllables _ _ _ _ Nothing  ) = showFirstSyllable s
  show s@(Syllables _ _ _ _ (Just ss)) = showFirstSyllable s ++ " " ++ show ss

type P = GenParser Char ()


-- THE PARSER
onset' :: [String] -> P Syllables
onset' []             = fail "invalid onset somewhere"
onset' (onset:onsets) = let withOnset = do stress1 <- stress
                                           string onset
                                           afterOnset onset stress1
                        in do maybeParsed <- optionMaybe (try withOnset)
                              fromMaybe (onset' onsets) (fmap return maybeParsed)

afterOnset :: String -> Stress -> P Syllables
afterOnset ons stress1 = do stress2 <- stress
                            nuc <- nucleus
                            (cod, next) <- coda' ""
                            let stress = if stress1 == NoStress
                                         then stress2
                                         else stress1
                            return $ Syllables ons nuc cod stress next

nucleus :: P String
nucleus = choice validNucleiParsers

syllables :: P Syllables
syllables = onset' validOnsets

validCodaParser :: (String, Maybe Syllables) -> P (String, Maybe Syllables)
validCodaParser codaPair@(coda, ss) = let validCoda = coda `elem` validCodas
                                      in if validCoda
                                         then return codaPair
                                         else fail "invalid coda somewhere"

stress :: P Stress
stress = choice [ char 'ˈ' >> return Primary
                , char 'ˌ' >> return Secondary
                , return NoStress
                ]

coda' :: String -> P (String, Maybe Syllables)
coda' s = do end <- optionMaybe eof
             if isJust end
               then validCodaParser (s, Nothing)
               else do rest <- optionMaybe (try syllables)
                       if isJust rest
                         then validCodaParser (s, rest)
                         else do c <- anyChar
                                 coda' (s ++ [c])
