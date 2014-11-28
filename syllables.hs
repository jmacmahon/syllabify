import Text.ParserCombinators.Parsec
import Data.Maybe ( isJust )

trousers = "traʊzəz"
improvements = "ɪmpruːvmənts"

vowels :: [Char]
vowels = "aʊəɪuːə"

consonants :: [Char]
consonants = "trzmprvns"

type Onset = String
type Nucleus = String
type Coda = String
data Syllable = Syllable Onset Nucleus Coda deriving (Show, Eq)

type Word = [Syllable]

filledNucleusP :: Syllable -> Bool
filledNucleusP (Syllable _ n _) = not (n == [])

getFirstSyllable' :: String -> Syllable -> (String, Syllable)
getFirstSyllable' ps s | filledNucleusP s = (ps, s)
getFirstSyllable' (p:ps) (Syllable ons "" "") | p `elem` vowels = getFirstSyllable' ps (Syllable ons [p] "")
                                              | otherwise       = getFirstSyllable' ps (Syllable (ons ++ [p]) "" "")
--getFirstSyllable' (p:ps) (Syllable ons "" "") | p `elem` 

data Syllables = Syllables { syllOnset :: String, syllNucleus :: String, syllCoda :: String, syllNext :: Maybe Syllables }
                 deriving (Show, Eq)

type P = GenParser Char ()

-- TODO: cluster checking
onset :: P String
onset = many (noneOf vowels)

-- TODO: dipthongs, syllabic consonants
nucleus :: P String
nucleus = many1 (oneOf vowels)

-- TODO: cluster checking
coda :: P String
coda = many (noneOf vowels)

syllables :: P Syllables
syllables = do ons <- onset
               nuc <- nucleus
               (cod, next) <- coda' ""
               return $ Syllables ons nuc cod next

coda' :: String -> P (String, Maybe Syllables)
coda' s = do end <- optionMaybe eof
             if isJust end
               then return (s, Nothing)
               else do rest <- optionMaybe (try syllables)
                       if isJust rest
                         then return (s, rest)
                         else do c <- anyChar
                                 coda' (s ++ [c])
                       

-- parsec test
one :: P String
one = (<|> string "testing") $ try $ do s <- string "test"
                                        _ <- eof
                                        return s
