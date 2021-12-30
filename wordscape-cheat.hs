import System.IO (readFile)
import System.Exit (exitSuccess)
import Data.Char (toLower, isAlpha)
import Data.List (permutations)

type Dictionary = [String]

actualWords :: [String] -> Dictionary -> [String]
actualWords x y = filter (`elem` y) x


lowercase :: String -> [Char] 
lowercase = map toLower


onlyLetters :: String -> Bool
onlyLetters = all isAlpha 


possibleWords :: String -> Dictionary -> [String]
possibleWords x = actualWords (permutations x)


-- check if word is only alphabet, turn to lowercase
cleanDictionary :: Dictionary -> Dictionary
cleanDictionary x = filter onlyLetters (map lowercase x)


loadDictionary :: String -> Dictionary
loadDictionary path = do
    contents <- readFile path
    cleanDictionary (lines contents)


main :: IO ()
main = do
    -- loading dictionary 
    let dictPath = "/usr/share/dict/american-english"
    
    dictionary <- loadDictionary dictPath

    putStrLn "Please type the letters involved (without spaces):"
    givenChars <- getLine

    let inputData = lowercase givenChars

    if length inputData < 3

        then do
            putStrLn "Invalid input, need at least three characters"
            exitSuccess

        else do
            let solutions = possibleWords inputData dictionary
            let numSols = length solutions
            let header = "There are " ++ show numSols ++ " possible solutions: "

            putStrLn (replicate (length header) '=')
            putStrLn header
            mapM_ putStrLn solutions