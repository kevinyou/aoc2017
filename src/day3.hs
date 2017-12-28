import Data.List.Split (splitOn)

-- cycles east, north, west, south, ...
unweighted_dirs = cycle [(1,0), (0,-1), (-1,0), (0,1)]

-- places for spiral
-- e.g. east 1, north 1, west 2, south 2, east 3, north 3, west 4..
dirs = concat [replicate n pair | (n, pair) <- zip doubled unweighted_dirs]
    where doubled = concat $ map (replicate 2) [1,2..]

-- more helpers
tupleAdd (x0, y0) (x1, y1) = (x0+x1, y0+y1)

-- part a
-- place n at cursor, then place n+1 at cursor+d
grid = place 1 (0,0) dirs
    where place n cursor (d:ds) = (n, cursor) : (place (succ n) (tupleAdd cursor d) ds)

-- part b
getValue (x, y) stored =
    case lookup (x, y) stored of
        Just v -> v
        Nothing -> 0

adjacent (x, y) = [(x+dx, y+dy) | dx <- [-1,0,1], dy <- [-1,0,1]]

placeAdj cursor (d:ds) stored = (cursor,  n) : (placeAdj (tupleAdd cursor d) ds ((cursor, n) : stored))
    where n = sum [value | value <- map (flip getValue stored) (adjacent cursor)]
gridAdj = placeAdj (0,0) ((0,0) : dirs) [((0,0), 1)]

day3a n = let Just (x,y) = lookup n grid in
    abs x + abs y

day3b n = head $ filter (>n) $ map snd $ gridAdj

main = do
    rawStr <- getContents
    let number = read rawStr :: Int
    print $ day3a number
    print $ day3b number

