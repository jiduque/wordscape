import Data.Char (isAlpha)
import Data.List (subsequences, permutations, sort, group, head)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (exitSuccess)


type Dictionary = [String]

onlyWords :: [String] -> Dictionary -> [String]
onlyWords x y = filter (`elem` y) x

-- TODO: subsequences is not right bc it doesn't do all combinations
allCombinations :: [a] -> [[a]]
allCombinations ns = filter ((>2).length) (subsequences ns ++ permutations ns)

possibleWords :: String -> Dictionary -> [String]
possibleWords x = onlyWords (allCombinations x)

onlyLetters :: String -> Bool
onlyLetters = all isAlpha

removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = map head . group . sort

main :: IO ()
main = do
    -- loading dictionary 
    let dictPath = "/usr/share/dict/american-english"
    text <- TIO.readFile dictPath
    let dictionary = removeDuplicates $ filter onlyLetters (map T.unpack $ T.lines $ T.toLower text)

    -- process data
    putStrLn "Please type the letters involved (without spaces):"
    givenChars <- getLine

    let inputData = T.unpack $ T.toLower $ T.pack givenChars

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