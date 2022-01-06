import Data.Char (isAlpha)
import Data.List (sort, group, head)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (exitSuccess)
import System.Environment (getArgs)


type Dictionary = S.Set String
type LetterPermutations = [String]
type Words = [String]

onlyWords :: LetterPermutations -> Dictionary -> Words
onlyWords xs y = filter (`S.member` y) xs

removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = map head . group . sort

cartesianProduct :: Int -> [a] -> [[a]]
cartesianProduct k xs   | k == 2       = [[x, y] | x <- xs, y <- xs]
                        | k == 3       = [x:y |  x <- xs, y <- cartesianProduct (k-1) xs]
                        | k > 3        = [ x:y |  x <- xs, y <- cartesianProduct (k-1) xs] ++ cartesianProduct (k-1) xs
                        | otherwise    = [[]]

counter :: String -> [Int]
counter xs = map (\c -> length $ filter (== c) xs) ['a'..'z']

validishString :: [Int] -> [Int] -> Bool
validishString xs ys = all (uncurry (>=)) (zip xs ys)

allCombinations :: String -> LetterPermutations
allCombinations xs = do
    let k = length xs
    let wordVec = counter xs
    removeDuplicates $ filter (validishString wordVec . counter) $ cartesianProduct k (sort xs)

possibleWords :: String -> Dictionary -> Words
possibleWords x = onlyWords (allCombinations x)

onlyLetters :: String -> Bool
onlyLetters = all isAlpha


main :: IO ()
main = do
    -- loading dictionary 
    let dictPath = "/usr/share/dict/american-english"
    text <- TIO.readFile dictPath
    let dictionary = S.fromAscList $ removeDuplicates $ filter onlyLetters (map T.unpack $ T.lines $ T.toLower text)
    
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

            putStrLn header
            putStrLn (replicate (length header) '=')
            mapM_ putStrLn solutions