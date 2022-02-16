module Wordscape where 

import Data.Char (isAlpha)
import Data.List (sort, group, head)
import qualified Data.Set as S
import qualified Data.Text as T


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

makeDict :: T.Text -> Dictionary
makeDict text = S.fromAscList $ removeDuplicates $ filter onlyLetters (map T.unpack $ T.lines $ T.toLower text)
