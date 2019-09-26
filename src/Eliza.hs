module Eliza where

import Control.Monad    -- guard no do-notation
import Data.Char        -- toLower
import System.Random
import Text.Regex
-- http://hackage.haskell.org/package/regex-compat-0.95.1/docs/Text-Regex.html
-- https://gabebw.com/blog/2015/10/11/regular-expressions-in-haskell
-- https://mail.haskell.org/pipermail/haskell-cafe/2010-May/078100.html

type Pattern = (String, [String])
type RegexPattern = (Regex, [String])

-- funcao que escolhe o padrao correto de acordo com o input
-- cada tupla na lista de padrao eh testada recursivamente
-- o vetor de padroes garante pelo menos uma resposta
getPattern :: String -> [RegexPattern] -> RegexPattern
getPattern input (x:xs)
    | match == Nothing  = getPattern input xs
    | otherwise         = x
        where match = matchRegexAll (fst x) input

-- converte o vetor de padroes em expressoes regulares
regexify :: [Pattern] -> [RegexPattern]
regexify [] = []
regexify xs = [(mkRegexWithOpts (fst x) True True, snd x) | x <- xs]

-- seleciona uma resposta aleatoria de acordo com o padrao selecionad
pickResponse :: RandomGen g => RegexPattern -> g -> String
pickResponse pattern randGen = response
    where
        response = possibleResponses !! rand
        rand = fst (randomR (0, (length possibleResponses) - 1) randGen)
        possibleResponses = snd pattern

-- responde a um determinado input
respond :: RandomGen g => String -> g -> String
respond input randGen = answer
    where
        answer = substitute (fst matchingPattern) input chosenResponse
        chosenResponse = pickResponse matchingPattern randGen
        matchingPattern = getPattern input (regexify patterns)

-- complementa a resposta com parte do discurso presente no input
-- atraves de referencia pela expressao regular (\\1)
substitute :: Regex -> String -> String -> String
substitute regex input replaceText = subRegex regex input replaceText

-- deixa todos os caracteres em caixa baixa
toLowerString :: String -> String
toLowerString palavra = [toLower n | n <- palavra]

patterns :: [Pattern]
patterns = [
    (
        "i need (.*)",
        [
            "Why do you need \\1?",
            "Would it really help you to get \\1?",
            "Are you sure you need \\1?"
        ]
    ),
    (
        "why don't you (.*)",
        [
            "Do you really think I don't \\1?",
            "Perhaps eventually I will \\1",
            "Do you really want me to \\1?"
        ]
    ),
    (
        "why can't I (.*)",
        [
            "Do you think you should be able to \\1?",
            "If you could \\1, what would you do?",
            "I don't know -- why can't you \\1?",
            "Have you really tried?"
        ]
    ),
    (
        "i can't (.*)",
        [
            "How do you know you can't \\1?",
            "Perhaps you could \\1 if you tried.",
            "What would it take for you to \\1?"
        ]
    ),
    (
        "i am (.*)",
        [
            "Did you come to me because you are \\1?",
            "How long have you been \\1?",
            "How do you feel about being \\1?"
        ]
    ),
    (
        "i'm (.*)",
        [
            "How does being \\1 make you feel?",
            "Do you enjoy being \\1?",
            "Why do you tell me you're \\1?",
            "Why do you think you're \\1?"
        ]
    ),
    (
        "are you (.*)",
        [
            "Why does it matter whether I am \\1?",
            "Would you prefer it if I were not \\1?",
            "Perhaps you believe I am \\1.",
            "I may be \\1 -- what do you think?"
        ]
    ),
    (
        "what (.*)",
        [
            "Why do you ask?",
            "How would an answer to that help you?",
            "What do you think?"
        ]
    ),
    (
        "how (.*)",
        [
            "How do you suppose?",
            "Perhaps you can answer your own question.",
            "What is it you're really asking?"
        ]
    ),
    (
        "because (.*)",
        [
            "Is that the real reason?",
            "What other reasons come to mind?",
            "Does that reason apply to anything else?",
            "If \\1, what else must be true?"
        ]
    ),
    (
        "(.*) sorry (.*)",
        [
            "There are many times when no apology is needed.",
            "What feelings do you have when you apologize?"
        ]
    ),
    (
        "hello(.*)",
        [
            "Hello... I'm glad you could drop by today.",
            "Hi there... how are you today?",
            "Hello, how are you feeling today?"
        ]
    ),
    (
        "i think (.*)",
        [
            "Do you doubt \\1?", 
            "Do you really think so?", 
            "But you're not sure \\1?"
        ]
    ),
    (
        "(.*) friend (.*)",
        [
            "Tell me more about your friends.",
            "When you think of a friend, what comes to mind?",
            "Why don't you tell me about a childhood friend?"
        ]
    ),
    (
        "yes", 
        [
            "You seem quite sure.", 
            "OK, but can you elaborate a bit?"
        ]
    ),
    (
        "(.*) computer(.*)",
        [
            "Are you really talking about me?",
            "Does it seem strange to talk to a computer?",
            "How do computers make you feel?",
            "Do you feel threatened by computers?"
        ]
    ),
    (
        "is it (.*)",
        [
            "Do you think it is \\1?",
            "Perhaps it's \\1 -- what do you think?",
            "If it were \\1, what would you do?",
            "It could well be that \\1."
        ]
    ),
    (
        "it is (.*)",
        [
            "You seem very certain.",
            "If I told you that it probably isn't \\1, what would you feel?"
        ]
    ),
    (
        "can you (.*)",
        [
            "What makes you think I can't \\1?",
            "If I could \\1, then what?",
            "Why do you ask if I can \\1?"
        ]
    ),
    (
        "can I (.*)",
        [
            "Perhaps you don't want to \\1.",
            "Do you want to be able to \\1?",
            "If you could \\1, would you?"
        ]
    ),
    (
        "you are (.*)",
        [
            "Why do you think I am \\1?",
            "Does it please you to think that I'm \\1?",
            "Perhaps you would like me to be \\1.",
            "Perhaps you're really talking about yourself?"
        ]
    ),
    (
        "you're (.*)",
        [
            "Why do you say I am \\1?",
            "Why do you think I am \\1?",
            "Are we talking about you, or me?"
        ]
    ),
    (
        "i don't (.*)",
        [
            "Don't you really \\1?", 
            "Why don't you \\1?", 
            "Do you want to \\1?"
        ]
    ),
    (
        "i feel (.*)",
        [
            "Good, tell me more about these feelings.",
            "Do you often feel \\1?",
            "When do you usually feel \\1?",
            "When you feel \\1, what do you do?"
        ]
    ),
    (
        "i have (.*)",
        [
            "Why do you tell me that you've \\1?",
            "Have you really \\1?",
            "Now that you have \\1, what will you do next?"
        ]
    ),
    (
        "i would (.*)",
        [
            "Could you explain why you would \\1?",
            "Why would you \\1?",
            "Who else knows that you would \\1?"
        ]
    ),
    (
        "is there (.*)",
        [
            "Do you think there is \\1?",
            "It's likely that there is \\1.",
            "Would you like there to be \\1?"
        ]
    ),
    (
        "my (.*)",
        [
            "I see, your \\1.",
            "Why do you say that your \\1?",
            "When your \\1, how do you feel?"
        ]
    ),
    (
        "you (.*)",
        [
            "We should be discussing you, not me.",
            "Why do you say that about me?",
            "Why do you care whether I \\1?"
        ]
    ),
    (
        "why (.*)", 
        [
            "Why don't you tell me the reason why \\1?", 
            "Why do you think \\1?"
        ]
    ),
    (
        "i want (.*)",
        [
            "What would it mean to you if you got \\1?",
            "Why do you want \\1?",
            "What would you do if you got \\1?",
            "If you got \\1, then what would you do?"
        ]
    ),
    (
        "(.*) mother(.*)",
        [
            "Tell me more about your mother.",
            "What was your relationship with your mother like?",
            "How do you feel about your mother?",
            "How does this relate to your feelings today?",
            "Good family relations are important."
        ]
    ),
    (
        "(.*) father(.*)",
        [
            "Tell me more about your father.",
            "How did your father make you feel?",
            "How do you feel about your father?",
            "Does your relationship with your father relate to your feelings today?",
            "Do you have trouble showing affection with your family?"
        ]
    ),
    (
        "(.*) child(.*)",
        [
            "Did you have close friends as a child?",
            "What is your favorite childhood memory?",
            "Do you remember any dreams or nightmares from childhood?",
            "Did the other children sometimes tease you?",
            "How do you think your childhood experiences relate to your feelings today?"
        ]
    ),
    (
        "(.*)?",
        [
            "Why do you ask that?",
            "Please consider whether you can answer your own question.",
            "Perhaps the answer lies within yourself?",
            "Why don't you tell me?"
        ]
    ),
    (
        "quit",
        [
            "Thank you for talking with me.",
            "Good-bye.",
            "Thank you, that will be $150.  Have a good day!"
        ]
    ),
    (
        "(.*)",
        [
            "Please tell me more.",
            "Let's change focus a bit... Tell me about your family.",
            "Can you elaborate on that?",
            "Why do you say that \\1?",
            "I see.",
            "Very interesting.",
            "\\1.",
            "I see.  And what does that tell you?",
            "How does that make you feel?",
            "How do you feel when you say that?"
        ]
    )]

talkToEliza :: IO ()
talkToEliza = do
    g       <- newStdGen
    input   <- getLine
    
    let speech = toLowerString input
 
    if elem speech ["bye", "goodbye", "see you", "see ya"]
        then return ()
        else do let answer = respond speech g
                print answer
                talkToEliza

main :: IO ()
main = do
    putStrLn "Welcome! I am Eliza and I am here to help you :)"
    putStrLn "Say bye if you no longer want to talk to me"

    talkToEliza
