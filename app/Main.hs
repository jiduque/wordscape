module Main where 


import Wordscape (Dictionary, possibleWords, makeDict)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

loadDictionary :: FilePath -> IO Dictionary
loadDictionary fp = do
    text <- TIO.readFile fp
    let dict = makeDict text
    return dict

-- add functionality to handle comman dine args instead of promptiong user

main :: IO ()
main = do
    -- loading dictionary 
    let dictPath = "/usr/share/dict/american-english"
    dictionary <- loadDictionary dictPath
    
    -- process data
    putStrLn "Please type the letters involved (without spaces):"
    givenChars <- getLine 
    let inputData = T.unpack $ T.toLower $ T.pack givenChars

    if length inputData < 3

        then do
            putStrLn "Invalid input, need at least three characters"

        else do
            let solutions = possibleWords inputData dictionary
            let numSols = length solutions
            let header = "There are " ++ show numSols ++ " possible solutions:"

            putStrLn header
            putStrLn (replicate (length header) '=')
            mapM_ putStrLn solutions