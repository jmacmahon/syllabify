module Linguisticks.Util where

import Data.Maybe ( isNothing, fromJust )
import Text.ParserCombinators.Parsec ( GenParser, many, noneOf, char, (<|>), optionMaybe, eof, runParser )
import Text.Parsec.Prim ( ParsecT )
import Text.Parsec.Error ( ParseError )
import qualified Data.Set as S
type Set = S.Set

sequenceSecond :: Monad m => [(a, m b)] -> m [(a, b)]
sequenceSecond ms = let (as, mbs) = unzip ms
                        mb = sequence mbs
                    in do bs <- mb
                          return $ zip as bs

choose :: Int -> [a] -> [[a]]
choose 0 _  = [[]]
choose n ss = let splits = [0..(length ss - 1)]
              in do split <- splits
                    let (top, bot) = splitAt split ss
                    rest <- choose (n-1) (tail bot)
                    return $ (head bot):rest

fromRight :: Either a b -> b
fromRight (Right b) = b

fromLeft :: Either a b -> a
fromLeft (Left b) = b

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

(<<) :: Monad m => m a -> m b -> m a
(<<) a b = do { v <- a ; b ; return v }

maybeP :: ParsecT s u m (Maybe a) -> ParsecT s u m a
maybeP parser = do parsed <- parser
                   maybe (fail "got nothing") return parsed

-- ADT for a Syllable.
data Stress = NoStress | Secondary | Primary deriving (Eq, Show, Ord, Enum)

primaryStress = 'ˈ'
secondaryStress = 'ˌ'

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

-- Quick parser for Read Syllables
readParserMaybe :: P (Maybe Syllables)
readParserMaybe = let stripNull :: String -> String
                      stripNull = filter (/= head nullSign)

                      wordNoNull :: P String
                      wordNoNull = fmap stripNull $ many $ noneOf " ()"

                      oneSyllable :: P Syllables
                      oneSyllable = do stress <- (char primaryStress >> return Primary) <|>
                                                 (char secondaryStress >> return Secondary) <|>
                                                 (return NoStress)
                                       char '('
                                       ons <- wordNoNull << char ' '
                                       nuc <- wordNoNull << char ' '
                                       cod <- wordNoNull << char ')'
                                       optionMaybe $ char ' '
                                       next <- readParserMaybe
                                       return $ Syllables ons nuc cod stress next
                  in (eof >> return Nothing) <|> fmap Just oneSyllable

instance Read Syllables where
  readsPrec _ ss = [(sylls, "")]
    where sylls = let parsed = runParser (maybeP readParserMaybe) zeroState "" ss
                  in either (const $ error "no parse") id parsed

data Word a = Word { wordSpelling :: String, wordTranscription :: String, wordSyllables :: a }
            deriving (Show, Eq)

type ParsedWord a = Word (Either ParseError a)
