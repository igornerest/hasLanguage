module NBClassifier where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Concurrent   -- MVars
import Data.List            -- maximumBy
import Data.Ord             -- comparing

type Tokens = [String]
type Class = String
type ClassWord = (Class, String)
type ClassTokens = (Class, Tokens)

newtype ClassCount  = ClassCount (MVar (M.Map Class Double))
newtype BigDoc      = BigDoc     (MVar (M.Map Class Tokens))
newtype Prior       = Prior      (MVar (M.Map Class Double))
newtype Likelihood  = Likelihood (MVar (M.Map ClassWord Double))
newtype Vocabulary  = Vocabulary (MVar (S.Set String))

-- dicionario vazio para contagem de classes
-- as chaves sao as classes e os valores sao as frequencias
newClassCount :: IO ClassCount
newClassCount = do  mcc <- newMVar M.empty
                    return (ClassCount mcc)

-- incrementa a frequencia de uma classe no dicionario
incrementClass :: ClassCount -> Class -> IO ()
incrementClass (ClassCount mcc) classe =  do  
    cc <- takeMVar mcc
    putMVar mcc (M.insertWith (+) classe 1.0 cc)

-- recupera a frequencia de uma classe no dicionario
classFrequency :: ClassCount -> Class -> IO (Double)
classFrequency (ClassCount mcc) classe = do
    cc <- takeMVar mcc
    putMVar mcc cc
    return (getFreq $ M.lookup classe cc)
        where
            getFreq (Just a) = a
            getFreq Nothing  = 0.0

-- conta a frequencia de todas as classes
countAllClasses :: ClassCount -> IO (Double)
countAllClasses (ClassCount mcc) = do
    cc <- takeMVar mcc
    putMVar mcc cc
    return (M.foldl (+) 0.0 cc)

-- recupera as classes adicionadas
getAllClasses :: ClassCount -> IO ([Class])
getAllClasses (ClassCount mcc) = do
    cc <- takeMVar mcc
    putMVar mcc cc
    return (M.foldlWithKey (\ks k _ -> k:ks) [] cc)

----------------------------------------------------------------------------------
-- dicionario vazio que armazena tokens de cada classe
-- as chaves sao as classes e os valores sao listas de tokens
newBigDoc :: IO BigDoc
newBigDoc = do  mbd <- newMVar M.empty
                return (BigDoc mbd)

-- adiciona tokens a lista de tokens de alguma classe
appendWords :: BigDoc -> Class -> Tokens -> IO ()
appendWords (BigDoc mbd) classe tokens = do  
    bd <- takeMVar mbd
    putMVar mbd (M.insertWith (++) classe tokens bd)

-- conta a frequencia de um token em uma classe especifica
countWord :: BigDoc -> Class -> String -> IO (Int)
countWord (BigDoc mbd) classe word = do
    bd <- takeMVar mbd
    putMVar mbd bd
    return (count word $ M.lookup classe bd)
        where
            count _ Nothing     = 0
            count w (Just ws)   = length $ filter (== w) ws

-- conta a quantidade de tokens de uma classe especifica
countWords :: BigDoc -> Class -> IO (Int)
countWords (BigDoc mbd) classe = do
    bd <- takeMVar mbd
    putMVar mbd bd
    return (count $ M.lookup classe bd)
        where
            count Nothing       = 0
            count (Just tokens) = length tokens 

----------------------------------------------------------------------------------
-- dicionario vazio que armazena a probabilidade a priori de uma classe
-- as chaves sao as classes e os valores sao as probabilidades
newPrior :: IO Prior
newPrior = do   mp <- newMVar M.empty
                return (Prior mp)

-- atualiza a probabilidade a priori de todas as classes 
-- de acordo com a contagem de classes
updatePrior :: Prior -> ClassCount -> IO ()
updatePrior prior classCount = do
    classes <- getAllClasses classCount
    updateClassPrior prior classCount classes

-- funcao auxiliar que atualiza a prob a priori para cada classe
-- que eh dada pela porcentagem de corpora treinados que estao em uma classe
updateClassPrior :: Prior -> ClassCount -> [Class] -> IO ()
updateClassPrior _ _ [] = return ()
updateClassPrior (Prior mp) classCount (c:cs) = do  
    prior   <- takeMVar mp
    nDoc    <- countAllClasses classCount
    nClass  <- classFrequency classCount c
    putMVar mp (M.insert c (nClass / nDoc) prior)
    updateClassPrior (Prior mp) classCount cs

-- recupera a probabilidade a priori de uma classe
classPrior :: Prior -> Class -> IO (Double)
classPrior (Prior mp) classe = do
    prior <- takeMVar mp
    putMVar mp prior
    return (getPrior $ M.lookup classe prior)
        where
            getPrior Nothing    = 0
            getPrior (Just a)   = a

----------------------------------------------------------------------------------
-- conjunto vazio de tokens unicos
-- representa o vocabulario dos dados de treinos
newVocabulary :: IO Vocabulary 
newVocabulary = do  v <- newMVar S.empty
                    return (Vocabulary v)

-- adiciona uma lista de tokens ao vocabulario, evitando repeticao
addVocabulary :: Vocabulary -> [String] -> IO ()
addVocabulary _ []  = return ()
addVocabulary (Vocabulary mv) (w:ws) = do
    v <- takeMVar mv
    putMVar mv (S.insert w v)
    addVocabulary (Vocabulary mv) ws

-- retorna o tamanho do vocabulario
countVocabulary :: Vocabulary -> IO (Int)
countVocabulary (Vocabulary mv) = do
    v <- takeMVar mv 
    putMVar mv v
    return (S.size v)

-- retorna uma lista com tokens unicos obtidos do treinamento
getVocabulary :: Vocabulary -> IO ([String])
getVocabulary (Vocabulary mv) = do
    v <- takeMVar mv
    putMVar mv v
    return (S.foldl (\ss s -> s:ss) [] v)
    
----------------------------------------------------------------------------------
-- dicionario vazio que armazena a verossimilhanca para todas as classes e tokens
newLikelihood :: IO Likelihood
newLikelihood = do  lh <- newMVar M.empty
                    return (Likelihood lh)

-- atualiza a verossimilhanca para todas as classes e tokens
-- as classes sao obtidas atraves do ClassCount
-- os tokens sao obtidos atraves do Vocabulary
updateLikelihood :: Likelihood -> ClassCount -> Vocabulary -> BigDoc -> IO ()
updateLikelihood likelihood classCount vocab bigDoc = do 
    classes <- getAllClasses classCount
    updateClassLikelihood likelihood bigDoc vocab classes

-- funcao auxiliar que atualiza a verossimilhanca recursivamente para cada classe
updateClassLikelihood :: Likelihood -> BigDoc -> Vocabulary -> [Class] -> IO ()
updateClassLikelihood _ _ _ [] = return ()
updateClassLikelihood likelihood bigDoc vocab (c:cs) = do
    words   <- getVocabulary vocab
    updateWordLikelihood likelihood bigDoc vocab c words
    updateClassLikelihood likelihood bigDoc vocab cs

-- funcao auxiliar que atualiza a verossimlhanca recursivamente para cada token
-- considerando uma classe especifica. A probabilidade P(w | c) eh a
-- quantidade de vezes que a palavra w aparece entre todas as palavras contidas 
-- nas corpora com rótulo c. Isso eh obtido com BigDoc
updateWordLikelihood :: Likelihood -> BigDoc -> Vocabulary -> Class -> [String] -> IO ()
updateWordLikelihood _ _ _ _ [] = return ()
updateWordLikelihood (Likelihood mlh) bigDoc vocab classe (w:ws) = do
    lh      <- takeMVar mlh
    nWords  <- countWords bigDoc classe
    nWord   <- countWord bigDoc classe w
    sVocab  <- countVocabulary vocab
    let prob = (fromIntegral (nWord + 1)) / (fromIntegral (nWords + sVocab))
    putMVar mlh (M.insert (classe, w) prob lh)
    updateWordLikelihood (Likelihood mlh) bigDoc vocab classe ws

-- Naive Bayes simplifica a formula da verossimilhanca assumindo que não há dependência 
-- das palavras e suas posições -> cada probabilidade P(w | c) é condicionalmente independente.
-- essa funcao calcula a formula geral P(w1 | c) * ... * P(wn | c) 
classLikelihood :: Likelihood -> Class -> [String] -> IO (Double)
classLikelihood _ _ [] = return (1.0)
classLikelihood (Likelihood mlh) classe (w:ws) = do
    lh <- takeMVar mlh
    putMVar mlh lh
    probNextWord <- classLikelihood (Likelihood mlh) classe ws
    let probCurWord = M.lookup (classe, w) lh
    return (probNextWord * (getLikelihood probCurWord))
        where
            getLikelihood Nothing    = 1.0
            getLikelihood (Just a)   = a

----------------------------------------------------------------------------------
-- leitura de um arquivo para uma lista de lista de tokens
-- cada lista sublista representa uma linha tokenizada
readData :: String -> IO [[String]]
readData filePath = do  document <- readFile filePath
                        return (map words $ lines document)

-- os dados sao formatos para tuplas, em que o primeiro elemento eh a classe
-- e o segundo eh uma lista de tokens classificado com aquela classe
formatData :: [[String]] -> [ClassTokens]
formatData []       = []
formatData (s:ss)   = (classe, tokens) : formatData ss
    where
        classe  = head s
        tokens  = tail s

-- atualiza os dicionarios para posterior treino
-- essa funcao pode ser chamada varias vezes e em diferentes momentos, pois usamos MVars
loadData :: ClassCount -> BigDoc -> Vocabulary -> [ClassTokens] -> IO ()
loadData _ _ _ [] = return ()
loadData classCount bigDoc vocab (ct:cts) = do
    let classe = fst ct
    let tokens = snd ct
    incrementClass classCount classe
    appendWords bigDoc classe tokens
    addVocabulary vocab tokens
    loadData classCount bigDoc vocab cts

-- funcao que contem leitura e formatacao dos dados, e atualizacao dos dicionarios
readAndLoadData :: String -> ClassCount -> BigDoc -> Vocabulary -> IO ()
readAndLoadData filePath cc bd vocab = do
    d <- readData filePath
    loadData cc bd vocab (formatData d)

-- treinamento. Simplesmente atualiza as probabilidades a priori e a verossimilhanca
train :: ClassCount -> BigDoc -> Vocabulary -> Prior -> Likelihood -> IO()
train classCount bigDoc vocab prior likelihood = do
    updatePrior prior classCount
    updateLikelihood likelihood classCount vocab bigDoc

testAll :: Likelihood -> Prior -> ClassCount -> [String] -> IO ([Class])
testAll _ _ _ [] = return []
testAll likelihood prior cc (d:ds) = do
    curClass    <- test likelihood prior cc d
    next        <- testAll likelihood prior cc ds
    return (curClass : next)

-- classificacao do texto. Recebe o dicinario de verossimilhanca e prob a priori 
-- ja calculados. Classes sao obtidas atraves de ClassCount
test :: Likelihood -> Prior -> ClassCount -> String -> IO (Class)
test likelihood prior classCount document = do
    let tokens = words document
    classes     <- getAllClasses classCount 
    classProb   <- testEachClass likelihood prior classes tokens
    let bestClass = snd $ maximumBy (comparing fst) classProb
    return (bestClass)

-- funcao auxiliar que calcula a probabilidade de classificacao para cada classe
-- retorna um vetor de tuplas. Primeiro elemento eh a probabilidade, segundo elemento
-- eh a classe
testEachClass :: Likelihood -> Prior -> [Class] -> [String] -> IO [(Double, Class)]
testEachClass _ _ [] _ = return []
testEachClass likelihood prior (c:cs) words = do
    probCurClass    <- getClassProbability likelihood prior c words
    probNextClasses <- testEachClass likelihood prior cs words
    return ((probCurClass, c) : probNextClasses) 

-- funcao auxiliar que calcula a probabilidade de uma lista de tokens pertencer 
-- a uma classe. 
getClassProbability :: Likelihood -> Prior -> Class -> [String] -> IO (Double)
getClassProbability likelihood prior classe words = do
    probPrior   <- classPrior prior classe
    probLh      <- classLikelihood likelihood classe words
    return (probPrior * probLh)

main :: IO ()
main = do
    -- inicializacao dos dicionarios/conjuntos
    cc      <- newClassCount
    bd      <- newBigDoc
    vocab   <- newVocabulary
    prior   <- newPrior
    lh      <- newLikelihood
    
    -- leitura e treinamento
    readAndLoadData "../data/train2.txt" cc bd vocab
    train cc bd vocab prior lh
    
    -- classificacao
    putStrLn "Escreva algo (em português). Tentaremos classificar :)"
    putStrLn "Para sair, digite 'batman'"
    classify lh prior cc
    
    return ()

classify :: Likelihood -> Prior -> ClassCount -> IO ()
classify lh prior cc = do
    text    <- getLine

    if text == "batman"
        then return ()
        else do result  <- test lh prior cc text
                print result
                classify lh prior cc
