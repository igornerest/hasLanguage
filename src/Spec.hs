module Spec where

import qualified NBClassifier   as NBC
import qualified NGrams         as NG
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
    defaultMain tests

tests :: TestTree
tests = (testGroup "Testes do HasLanguage" [naiveBayesTest, nGramsTest])

------------------------- Naive Bayes
naiveBayesTest = testGroup "Naive Bayes Test" testAccuracy

testAccuracy = [testCase ("Phrase -> " ++ d) (checkClassification d c) | (c, d) <- test_data_NB]

checkClassification doc classe = do cc      <- NBC.newClassCount
                                    bd      <- NBC.newBigDoc
                                    vocab   <- NBC.newVocabulary
                                    prior   <- NBC.newPrior
                                    lh      <- NBC.newLikelihood
                                
                                    NBC.readAndLoadData "../data/train2.txt" cc bd vocab
                                    NBC.train cc bd vocab prior lh
            
                                    result <- NBC.test lh prior cc doc
                                    assertEqual ("Phrase -> " ++ doc) classe result

------------------------- NGrams
nGramsTest = testGroup "NGrams Test" testTokenization

testTokenization = [testCase ("Phrase -> " ++ str) (checkTokenization str tk) | (str, tk) <- test_data_NG]

checkTokenization str tokenized = do assertEqual ("Phrase ->" ++ str) tokenized $ NG.getWords str

------------------------- Test Data
test_data_NB =  [   
                    ("pos", "acordei muito bem hoje"),
                    ("neg", "o trânsito está horrível"),
                    ("neg", "eu odeio trabalhar mais que 8 horas por dia"),
                    ("pos", "essa música está interessante"),
                    ("pos", "O emílio é um cara legal"),
                    ("neg", "Esse projeto deu bastante trabalho")
                ]

test_data_NG =  [
                    ("olá, como vai?", ["olá", "como", "vai"]),
                    ("Eu. Só. Sei. Sei? Nada sei: talvez", ["eu", "só", "sei", "sei", "nada", "sei", "talvez"]),
                    (".....?!!:e", ["e"]), 
                    ("", []),
                    (" ", []),
                    ("............... !", []),
                    ("33 tigres famintos", ["33", "tigres", "famintos"])
                ]
