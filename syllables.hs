import Text.ParserCombinators.Parsec

trousers = "traʊzəz"

vowels :: [Char]
vowels = "aʊə"

consonants :: [Char]
consonants = "trz"

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

type P = GenParser Char ()

onset :: P String
onset = many (noneOf vowels)

-- TODO: dipthongs, syllabic consonants
nucleus :: P String
nucleus = many (oneOf vowels)



-- parsec test
one :: P String
one = (<|> string "testing") $ try $ do s <- string "test"
                                        _ <- eof
                                        return s
