module Syllables ( parse
                 , trousers
                 , improvements
                 ) where

import Text.ParserCombinators.Parsec
import Data.Maybe ( isJust, fromMaybe, fromJust, isNothing )
import Debug.Trace ( trace )
import Control.Monad ( join )

-- Reading the CSV
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
             return $ Word spelling transcription transcription

readAndParse :: FilePath -> IO [Word (Either ParseError Syllables)]
readAndParse fp = do ws <- readFile fp
                     let words = fromRight $ runParser csvParser zeroState "" ws
                         parsed = map (fmap parseSyllables) words
                     return parsed

-- Writing the CSV
rowFormat :: Word (Either a Syllables) -> String
rowFormat w = let syllFormatted = fmap (either (const "ERROR") show) w
              in (wordSpelling syllFormatted)
                 ++ "," ++ (wordTranscription syllFormatted)
                 ++ "," ++ (wordSyllables syllFormatted)

fromRight :: Either a b -> b
fromRight (Right b) = b

fromLeft :: Either a b -> a
fromLeft (Left b) = b

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

-- Some example transcriptions
trousers = "trˈaʊzəz"
improvements = "ɪmprˈuːvmənts"
glimpsed = "glˈɪmpst"
table = "tɛɪbɫ̩"
rhythm = "rɪðm̩"

-- STRESS MARKERS SECTION
primaryStress = 'ˈ'
secondaryStress = 'ˌ'

-- ONSETS SECTION
singleOnsets = ["p", "b", "t", "d", "k", "g", "f", "v", "θ", "ð", "s", "z", "ʃ", "ʒ", "ʧ", "ʤ", "m", "n", "l", "r", "w", "j", "h"]
doubleOnsets = ["pl", "pr", "pj", "bl", "br", "bj", "tr", "tj", "tw", "dr", "dj", "dw", "kl", "kr", "kj", "kw", "gl", "gr", "gj", "gw", "mj", "nj", "fl", "fr", "fj", "vj", "θr", "θj", "θw", "sl", "sj", "sw", "sp", "st", "sk", "sm", "sn", "ʃr", "hj", "lj"]
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
data Syllables = Syllables { syllOnset :: String,
                             syllNucleus :: String,
                             syllCoda :: String,
                             syllStress :: Stress,
                             syllNext :: Maybe Syllables
                           } deriving (Eq)

syllsToList :: Syllables -> [(String, String, String, Stress)]
syllsToList s@(Syllables ons nuc cod stress next) | isNothing next = [(ons, nuc, cod, stress)]
                                                  | otherwise      = (ons, nuc, cod, stress):(syllsToList $ fromJust next)

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

type P = GenParser Char SyllState

data SyllState = SyllState { nextStress :: Stress
                           } deriving (Show, Eq, Ord)
zeroState :: SyllState
zeroState = SyllState NoStress

-- Convenience functions for the parser and representations
parseSyllables :: String -> Either ParseError Syllables
parseSyllables s = let parser :: P Syllables
                       parser = do sylls <- syllables
                                   maybe (fail "nothing to parse") return sylls
                   in runParser parser zeroState "" s

traceParser :: String -> P ()
traceParser prefix = do st <- getState
                        input <- getInput
                        let output = prefix ++ ": " ++ show input ++ " " ++ show st
                        trace output $ return ()

-- THE PARSER
stressString :: String -> P String
stressString = let setStress c | c == primaryStress   = updateState $ \s -> s { nextStress = Primary }
                               | c == secondaryStress = updateState $ \s -> s { nextStress = Secondary }
                               | otherwise            = fail "stress setting went wrong.  this is a bug."

                   checkStress :: P ()
                   checkStress = do optionMaybe $ do s <- char primaryStress <|> char secondaryStress
                                                     setStress s
                                    return ()

                   parseString :: String -> P String
                   parseString ""     = do checkStress
                                           return ""
                   parseString (s:ss) = do checkStress
                                           char s
                                           rest <- parseString ss
                                           return (s:rest)
               in parseString

popStress :: P Stress
popStress = do stress <- fmap nextStress getState
               updateState $ \s -> s { nextStress = NoStress }
               return stress

syllables :: P (Maybe Syllables)
syllables = (fmap Just $ onset) <|> (eof >> return Nothing)

onset :: P Syllables
onset = let onset' ons = try $ do stressString ons
                                  nucleus ons
        in choice $ map onset' validOnsets

nucleus :: String -> P Syllables
nucleus ons = do nuc <- choice validNucleiParsers
                 stress <- popStress
                 coda ons nuc stress

coda :: String -> String -> Stress -> P Syllables
coda ons nuc stress = let coda' cod = try $ do stressString cod
                                               next <- syllables
                                               return $ Syllables ons nuc cod stress next
                      in choice $ map (coda') validCodas
