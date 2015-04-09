module Linguisticks where

import qualified Linguisticks.Util as U
import qualified Linguisticks.Parser as Par
import qualified Linguisticks.CSV as C
import qualified Linguisticks.Statistics.Analysis as An
import qualified Linguisticks.Statistics.Processing as Pr

import Text.Parsec.Error ( ParseError )
import Text.Printf ( printf )

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

cmpSign :: Ordering -> Char
cmpSign LT = '<'
cmpSign EQ = '='
cmpSign GT = '>'

significance p | p < 0.005 = "Yes"
               | otherwise = "No"

format (n1, n2, p, cmp) | p < 0.005 = printf "| %s | %s | %s | %s %c %s | %.2e |\n" n1 n2 (significance p) n1 (cmpSign cmp) n2 p
                        | otherwise = printf "| %s | %s | %s | N/A | %.2e |\n" n1 n2 (significance p) p

formatConsonant (c, n1, n2, p, cmp) = printf "| %c | %s | %s | %s | %s %c %s | %.2e |\n" c n1 n2 (significance p) n1 (cmpSign cmp) n2 p

formatParam (c, n1, n2, p, cmp) = printf "| %s | %s | %s | %s | %s %c %s | %.2e |\n" c n1 n2 (significance p) n1 (cmpSign cmp) n2 p

printTests :: IO [(String, String, Double, Ordering)] -> IO ()
printTests res = do results <- res
                    sequence_ $ map format results

runTests pr = printTests $ fmap (An.tTest pr) parsed_all

formatConfidence (n, u, l) = printf "| %s | %.2f | %.2f |\n" n u l
formatParamConfidence (c, n, u, l) = printf "| %s | %s | %.3f | %.3f |\n" c n u l

printMeans :: [(String, Double, Double)] -> IO ()
printMeans results = sequence_ $ map formatConfidence results

runMeans pr = fmap (An.meanConfidenceIntervals 0.99 pr) parsed_all >>= printMeans

runParametrisedMeans pr params = do parsed <- parsed_all
                                    let results :: [(String, String, Double, Double)]
                                        results = do param <- params
                                                     let result = do r <- An.meanConfidenceIntervals 0.99 (pr param) parsed
                                                                     return $ (\(n, u, l) -> (param, n, u, l)) r
                                                     result
                                    sequence_ $ map formatParamConfidence results

runParametrisedTests pr params = do parsed <- parsed_all
                                    let results :: [(String, String, String, Double, Ordering)]
                                        results = do param <- params
                                                     let result = do r <- An.tTest (pr param) parsed
                                                                     return $ (\(n1, n2, p, o) -> (param, n1, n2, p, o)) r
                                                         filteredResult = filter (\(_, _, _, p, _) -> p < 0.005) result
                                                     filteredResult
                                    sequence_ $ map formatParam $ results

main = runParametrisedMeans (Pr.sampleConsonantRatio . head) (map return Pr.consonants)

{-
main = sequence_ $ map putStrLn [ons ++ nuc ++ cod | ons <- Par.validOnsets, nuc <- Par.validNuclei, cod <- Par.validCodas]
-}
