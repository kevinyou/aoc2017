import Data.List.Split (splitOn)

dividesEachOther x y = ((mod (max x y) (min x y)) == 0)
getQuotient (x:xs) =
    case filter (dividesEachOther x) xs of
        [] -> getQuotient xs
        [y] -> div (max x y) (min x y)

day2a xss = sum [maximum xs - minimum xs | xs <- xss]
day2b xss = sum [getQuotient xs | xs <- xss]

main = do
    rawStr <- getContents
    let rawLines = init $ splitOn "\n" rawStr
        lines = [splitOn "\t" line | line <- rawLines]
        numbers = map (map read) lines :: [[Int]]
        a = day2a numbers
        b = day2b numbers
    print a
    print b

