import Data.List.Split (splitOn)
import Data.List (sort, nubBy)

counts = foldr (\x acc -> count x acc : acc) []
    where count x dict = case lookup x dict of
            Just val -> (x, val+1)
            Nothing -> (x, 1)

removeDup = nubBy (\a b -> fst a == fst b)
valid xs = (length $ removeDup $ counts xs) == (length xs)

day4a xss = length $ filter valid xss
day4b xss = day4a $ map (map sort) xss

main = do
    rawStr <- getContents
    let rawLines = init $ splitOn "\n" rawStr
        myWords = map words rawLines
    print $ day4a myWords
    print $ day4b myWords

