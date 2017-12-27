import Data.Char (digitToInt)

matches n xs = [x | (x,y) <- zip xs $ drop n $ cycle xs, x == y]
day1a list = sum $ matches 1 list
day1b list = sum $ matches (div (length list) 2) list

main = do
    str <- getLine
    let a = day1a $ map digitToInt str
        b = day1b $ map digitToInt str
    print a
    print b

