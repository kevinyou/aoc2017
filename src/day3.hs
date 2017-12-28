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
place n cursor (d:ds) = (n, cursor) : (place (succ n) (tupleAdd cursor d) ds)
    where newCursor = tupleAdd cursor d

grid = place 1 (0,0) dirs

-- part b
myfind (ox, oy) stored =
    case lookup (ox, oy) stored of
        Just v -> v
        Nothing -> 0

place' cursor (d:ds) = cursor : (place' (tupleAdd cursor d) ds)
    where newCursor = tupleAdd cursor d

spiral = place' (0,0) dirs

adjacent (ox, oy) = [(ox+x, oy+y) | x <- [-1,0,1], y <- [-1,0,1]]

combined ((ox, oy):rest) stored =
    let values = [value | (x,y) <- (adjacent (ox,oy)), let value = myfind (x,y) stored]
        pair = ((ox, oy), sum values) in
        pair : (combined rest (pair : stored))

day3a n = let Just (x,y) = lookup n grid in
    abs x + abs y

-- TODO: rewrite with some sort of mutable?
day3b n = let mygrid = combined spiral [((0,0), 1)] in
    head $ filter (>n) $ map snd mygrid

main = do
    rawStr <- getContents
    let number = read rawStr :: Int
    print $ day3a number
    print $ day3b number

