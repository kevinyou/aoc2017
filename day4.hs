import Data.List.Split (splitOn)
import Data.List (sort, nubBy)

count (x:xs) dict =
    case lookup x dict of
        Just val -> (x, val+1) : count xs ((x, val+1) : dict)
        Nothing -> (x, 1) : count xs ((x, 1) : dict)
count [] dict = []

removeDup = nubBy (\a -> \b -> fst a == fst b)
valid xs = (length $ removeDup $ count xs []) == (length xs)

day4a xss = length $ filter valid xss
day4b xss = day4a $ map (map sort) xss

main = do
    rawStr <- getContents
    let rawLines = init $ splitOn "\n" rawStr
        myWords = map words rawLines
    print $ day4a myWords
    print $ day4b myWords

