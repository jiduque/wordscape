module Main where 


import Wordscape (Dictionary, Words, possibleWords, makeDict)
import System.Environment (getArgs)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T


getWordscapePossibilities :: String -> FilePath -> IO ()
getWordscapePossibilities givenChars dictPath = do
    text <- TIO.readFile dictPath
    let dictionary = makeDict text
    let inputChars = T.unpack $ T.toLower $ T.pack givenChars
    let solutions = possibleWords inputChars dictionary
    let numSols = length solutions
    let header = "There are " ++ show numSols ++ " possible solutions:"
    putStrLn header
    putStrLn (replicate (length header) '=') 
    mapM_ putStrLn solutions


main :: IO ()
main = do
    args <- getArgs 
    case args of 
        [chars] -> getWordscapePossibilities chars "/usr/share/dict/american-english"
        [chars, "-d", dict] -> getWordscapePossibilities chars dict
        _ -> putStrLn $ "Usage: will give possible solutions to a wordscape game given a string of characters\n" ++
                        "       wordscape letters\n" ++
                        "       wordscape letters -d dictionary"
