module NGrams where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List.Split  -- splitChar
import Data.Char        -- toLower
import System.Random    -- RandomGen
import System.IO

type NGram = Map [String] Double
type GramTuple = (Words, Double)
type Words = [String]

-- divide uma string em uma lista de palavras, considerando chars acentuados
-- semelhante a expressao regulares "[-'a-zA-ZÀ-ÖØ-öø-ÿ]+"
getWords :: String -> [String]
getWords str = wordsBy splitChar $ map toLower str

splitChar :: Char -> Bool
splitChar c | c == '-'              = False
            | c >= 'a' && c <= 'z'  = False
            | c >= 'A' && c <= 'Z'  = False
            | c >= 'À' && c <= 'Ö'  = False
            | c >= 'Ø' && c <= 'ö'  = False
            | c >= 'ø' && c <= 'ÿ'  = False
            | c >= '0' && c <= '9'  = False
            | otherwise             = True


-- retorna um dicionario de ngramas vazio, cujas chaves sao ngramas, sendo os
-- valores as frequencias correspondentes. 
-- Um ngrama eh representado como uma string de n palavras
nullGram :: NGram
nullGram = Map.fromList []

-- recupera a frequencia do ngrama no dicionario
-- se a frequencia nao existe retorna zero
getFreq :: Words -> NGram -> Double
getFreq wordSeq map = case Map.lookup wordSeq map of
                        Nothing     -> 0.0
                        Just value  -> value

-- recupera uma lista de ngramas de tamanho n
-- com uma frequencia especifica
getWithFreq :: NGram -> Int -> Double -> [GramTuple]
getWithFreq map nSize freq = filter (\(ws, f) -> f >= freq && length ws == nSize) mapTuples
    where
        mapTuples = Map.foldrWithKey (\k v ks -> (k, v):ks) [] map

-- recupera um ngrama a partir de um corpus
-- sao considerados ngramas de tamanho n
getNGram :: String -> Int -> NGram
getNGram corpus n = countGrams corpusWords n
    where
        corpusWords = getWords corpus

-- incrementa frequencia do ngrama no dicionario
incrementFreq :: Words -> NGram -> NGram
incrementFreq wordSeq oldMap = Map.insertWith (+) wordSeq 1.0 oldMap

-- conta os n-gramas para todos os tamanhos
-- para trigramas: (), (w), (w, w), (w, w, w)
countGrams :: [String] -> Int -> NGram
countGrams _ 0  = nullGram
countGrams [] _ = nullGram
countGrams xs n = Map.unionWith (+) (countGram xs n) (countGrams xs (n-1))

-- conta n-gramas de um tamanho especifico
countGram :: Words -> Int -> NGram
countGram [] _  = nullGram
countGram (w:ws) n 
    | length wordSeq == n   = incrementFreq wordSeq map
    | otherwise             = nullGram
        where
            wordSeq = take n (w:ws)
            map = countGram ws n

-- calcula a probabilidade para uma string
-- tomando como base um corpus e um ngrama de tamanho n
calcProbability :: String -> String -> Int -> Double
calcProbability phrase corpus n = phraseProb phWords phSize n newMap
    where
        phWords     = getWords phrase
        corpusWords = getWords corpus
        phSize      = length phWords
        newMap      = countGrams corpusWords n 

-- calcula a probabilidade para uma frase de tamanho seqSize
-- para um ngrama de tamanho n
phraseProb :: Words -> Int -> Int -> NGram -> Double
phraseProb (w:ws) seqSize n map
    | seqSize <= n  = ngramProb (w:ws) map
    | otherwise     = prob * phraseProb ws (seqSize - 1) n map
        where 
            prob    = ngramProb wordSeq map
            wordSeq = take n (w:ws)

-- calcula a aproximacao para o n-grama usando o seu prefixo, com  pressuposto de Markov
-- seja a frase "A B C", sua probabilidade é dada por
-- P("C" | "A", "B") = P("A", "B", "C") / P("B", "C")
ngramProb :: Words -> NGram -> Double
ngramProb words map = if prefixCount == 0 then 0 else probability
    where
        probability = ngramCount / prefixCount
        ngramCount  = getFreq words map
        prefixCount = getFreq (init words) map 

-- gera uma frase de um tamanho phSize considerando um ngrama de tamanho ngSize
-- TODO: mudar unigrams para everygrams
genPhrase :: RandomGen g => NGram -> Int -> Int -> g -> String
genPhrase map phSize ngSize randGen = phrase
    where
        phrase      = mostProbPhrase map prefix phSize ngSize randGen
        prefix      = getString $ fst (unigrams !! rand)
        unigrams    = getWithFreq map 1 1.0
        rand        = fst $ randomR (0, length unigrams) randGen

-- frase mais provavel de acordo com um prefixo
-- os tamanhos da frase e do ngrama sao especificados
mostProbPhrase :: RandomGen g => NGram -> String -> Int -> Int -> g -> String
mostProbPhrase _ _ 0 _ _ = []
mostProbPhrase map prefix phSize ngSize randGen = prefix ++ " " ++ nextWords
    where
        nextWords   = mostProbPhrase map prefix' (phSize-1) ngSize (snd randPair)
        prefix'     = mostProbWord unigrams [prefix] map 0.0 (fst randPair)
        unigrams    = getWithFreq map ngSize 1.0
        randPair    = randomR (0.0, 1.0) randGen

-- palavra mais provavel dado um prefixo e um limiar de probabilidade
mostProbWord :: [GramTuple] -> Words -> NGram -> Double -> Double -> String
mostProbWord [] _ _ _ _ = []
mostProbWord (t:ts) prefix map curProb threshold
    | prob >= threshold = getString $ fst t
    | otherwise         = mostProbWord ts prefix map prob threshold
        where
            prob = curProb + (ngramProb newGram map)
            newGram = prefix ++ (fst t)

-- transforma uma lista de strings em uma string espacada
getString :: Words -> String
getString = foldl (\acc x -> if acc == [] then x else acc ++ " " ++  x) []

main :: IO ()
main = do
    g <- newStdGen
    corpus <- readFile "../data/Resumos-aulas.txt"

    putStrLn "Digite o tamanho de sua frase"
    input   <- getLine

    let phSize = read input :: Int

    print $ genPhrase (getNGram corpus 3) phSize 1 g
