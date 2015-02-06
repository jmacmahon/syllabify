module Syllables ( parse
                 , trousers
                 , improvements
                 ) where

import Text.ParserCombinators.Parsec
import Data.Maybe ( isJust, fromMaybe )

trousers = "traʊzəz"
improvements = "ɪmpruːvmənts"
glimpsed = "glɪmpst"

vowels :: [Char]
vowels = "aʊəɪuːə"

consonants :: [Char]
consonants = "pbtdkgfvθðszʃʒʧʤmnŋlrwjh"

singleOnsets = ["p", "b", "t", "d", "k", "g", "f", "v", "θ", "ð", "s", "z", "ʃ", "ʒ", "ʧ", "ʤ", "m", "n", "l", "r", "w", "j", "h"]

doubleOnsets = ["pl", "pr", "pj", "bl", "br", "bj", "tr", "tj", "tw", "dr", "dj", "dw", "kl", "kr", "kj", "kw", "gl", "gr", "gj", "gw", "mj", "nj", "fl", "fr", "fj", "vj", "θr", "θj", "θw", "sl", "sj", "sw", "sp", "st", "sk", "sm", "sn", "ʃr", "hj", "lj"]

tripleOnsets = ["spl", "spr", "spj", "str", "stj", "skl", "skr", "skj", "skw"]

validOnsets :: [[Char]]
validOnsets = tripleOnsets ++ doubleOnsets ++ singleOnsets ++ [""]

singleCodas = ["p", "b", "t", "d", "k", "g", "f", "v", "θ", "ð", "s", "z", "ʃ", "ʒ", "ʧ", "ʤ", "m", "n", "ŋ", "l", "r"]

doubleCodas = ["pt", "pθ", "ps", "tθ", "ts", "kt", "ks", "kθ", "bd", "bz", "dz", "gd", "gz", "ʧt", "ʤd", "mp", "md", "mf", "mθ", "mz", "nt", "nd", "nʧ", "nʤ", "nθ", "ns", "nz", "ŋd", "ŋk", "ŋz", "ŋθ", "lp", "lt", "lk", "lb", "ld", "lʧ", "lʤ", "lm", "ln", "lf", "lv", "lθ", "ls", "lz", "lʃ", "ft", "fθ", "fs", "vd", "vz", "θt", "θs", "ðd", "ðz", "sp", "st", "sk", "zd", "ʃt", "ʒd", "rp", "rb", "rt", "rd", "rt", "rʧ", "rʤ", "rk", "rg", "rm", "rn", "rl"]

tripleCodas = ["pts", "pst", "pθs", "tst", "tθs", "dst", "kts", "kst", "ksθ", "kθs", "mpt", "mps", "mfs", "ntθ", "nts", "ndz", "nʧt", "nʤd", "nθs", "nst", "nzd", "ŋkt", "ŋkθ", "ŋks", "lpt", "lps", "lts", "lkt", "lks", "lbz", "ldz", "lʧt", "lʤd", "lmd", "lmz", "lnz", "lfs", "lfθ", "lvd", "lvz", "lθs", "lst", "fts", "fθs", "spt", "sps", "sts", "skt", "sks", "rmθ", "rpt", "rps", "rts", "rst", "rkt"]

quadCodas = ["mpts", "mpst", "lkts", "lpts", "lfθs", "ksts", "ksθs", "ntθs"]

validCodas :: [[Char]]
validCodas = [""] ++ singleCodas ++ doubleCodas ++ tripleCodas ++ quadCodas

validOnsetsParsers :: [P String]
validOnsetsParsers = map (try . string) validOnsets

data Syllables = Syllables { syllOnset :: String, syllNucleus :: String, syllCoda :: String, syllNext :: Maybe Syllables }
                 deriving (Eq)

parseSyllables :: String -> Either ParseError Syllables
parseSyllables = parse syllables ""

nullSign = "∅"
nullReplace s = if null s then nullSign else s

showFirstSyllable (Syllables ons nuc cod _) =    "(" ++ nullReplace ons
                                              ++ " " ++ nullReplace nuc
                                              ++ " " ++ nullReplace cod
                                              ++ ")"
instance Show Syllables where
  show s@(Syllables _ _ _ Nothing  ) = showFirstSyllable s
  show s@(Syllables _ _ _ (Just ss)) = showFirstSyllable s ++ " " ++ show ss

type P = GenParser Char ()

onset :: P String
onset = choice validOnsetsParsers

-- TODO: dipthongs, syllabic consonants
nucleus :: P String
nucleus = many1 (oneOf vowels)

syllables :: P Syllables
syllables = do ons <- onset
               nuc <- nucleus
               (cod, next) <- coda' ""
               return $ Syllables ons nuc cod next

validCodaParser :: (String, Maybe Syllables) -> P (String, Maybe Syllables)
validCodaParser codaPair@(coda, ss) = let validCoda = coda `elem` validCodas
                                      in if validCoda
                                         then return codaPair
                                         else fail "invalid coda somewhere"

coda' :: String -> P (String, Maybe Syllables)
coda' s = do end <- optionMaybe eof
             if isJust end
               then validCodaParser (s, Nothing)
               else do rest <- optionMaybe (try syllables)
                       if isJust rest
                         then validCodaParser (s, rest)
                         else do c <- anyChar
                                 coda' (s ++ [c])
                       

-- parsec test
one :: P String
one = choice [(try $ (string "test" >>= \s -> eof >> return s)), try (string "testing")]
