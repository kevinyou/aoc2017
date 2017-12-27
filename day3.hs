import Data.List.Split (splitOn)

-- generating directions
interleave (x:xs) (y:ys) = x : y : interleave xs ys
alt = 1 : map ((-1)*) alt
zeroes = [0,0..]
unweighted_dirs = zip (tail (interleave zeroes alt)) (interleave zeroes (tail alt))
doubled = interleave [1,2..] [1,2..]
dirs' = [(w*x, w*y) | ((x,y), w) <- zip unweighted_dirs doubled]

unwrap ((x,0):xs) =
    case abs x of
        1 -> (x,0) : unwrap xs
        _ -> case compare x 0 of
            LT -> (-1,0) : unwrap ((x+1,0) : xs)
            GT -> (1,0) : unwrap ((x-1,0) : xs)
unwrap ((0,x):xs) =
    case abs x of
        1 -> (0,x) : unwrap xs
        _ -> case compare x 0 of
            LT -> (0,-1) : unwrap ((0,x+1) : xs)
            GT -> (0,1) : unwrap ((0,x-1) : xs)

dirs = unwrap dirs'

-- more helpers
tupleAdd (x0, y0) (x1, y1) = (x0+x1, y0+y1)
partialSum (x:xs) = x : (map (tupleAdd x) (partialSum xs))

-- part a
grid = (1, (0,0)) : (zip [2,3..] dirs)
myfind (ox, oy) stored =
    case lookup (ox, oy) stored of
        Just v -> v
        Nothing -> 0

-- part b

spiral = (0,0) : (partialSum dirs)

adjacent (ox, oy) = [(ox+x, oy+y) | x <- [-1,0,1], y <- [-1,0,1]]

combined ((ox, oy):rest) stored =
    let values = [value | (x,y) <- (adjacent (ox,oy)), let value = myfind (x,y) stored]
        pair = ((ox, oy), sum values) in
        pair : (combined rest (pair : stored))

locAt n ((pos, (dx,dy)):xs) = 
    if pos == n then (dx,dy)
    else let (x,y) = locAt n xs in
        (x+dx, y+dy)

day3a n = let (x,y) = locAt n grid in
    abs x + abs y
day3b n = let mygrid = combined spiral [((0,0), 1)] in
    head $ filter (>n) $ map snd mygrid

main = do
    rawStr <- getContents
    let number = read rawStr :: Int
    print $ day3a number
    print $ day3b number

