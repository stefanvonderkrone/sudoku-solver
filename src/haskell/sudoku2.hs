import Data.List
import Data.Maybe
import Data.Time
import Control.DeepSeq

difference base xs = base \\ xs

{-
    from http://en.literateprograms.org/Quicksort_(Haskell)#chunk def:qsort3

    extended with comparator-function
-}
fastQsort :: (Ord a) => (a -> a -> Ordering) -> [a] -> [a]
fastQsort comp x = fastQsort' x []
    where
        fastQsort' [] y     = y
        fastQsort' [x] y    = x:y
        fastQsort' (x:xs) y = part xs [] [x] []
            where
                part [] l e g = fastQsort' l (e ++ (fastQsort' g y))
                part (z:zs) l e g
                    | comp z x == GT = part zs l e (z:g)
                    | comp z x == LT = part zs (z:l) e g
                    | otherwise      = part zs l (z:e) g

{-
    solveSudoku
    @param puzzle    [Just a, Nothing,...] the (one-dimensional) puzzle to solve
    @param base      [a,b,c,d,...] the possible values
    @param blockSize the width of each block
-}
solveSudoku :: (Eq a, Ord a) => [Maybe a] -> [a] -> Int -> Maybe [a]
solveSudoku puzzle base blockSize =
    -- get simplified solution or Nothing
    case (getSolution sortedPossibles puzzle range) of
        Just s    -> Just $ map (\(Just x) -> x) s
        otherwise -> Nothing
    where
        -- [0,1,2,3 .. lenght of puzzle]
        range = [0 .. (length puzzle)]
        -- comparator-function
        compPossibles (i1,p1) (i2,p2)
                    | length p1 > length p2 = GT
                    | length p1 < length p2 = LT
                    | otherwise             = EQ
        -- simple sorting function
        sortPossibles p = fastQsort compPossibles p
        -- (i,Nothing) -> (i,[list of possobles])
        toPossible (i,_) = (i, getTestValues puzzle base i blockSize)
        -- sorted list of tuples (i,[a])
        sortedPossibles = sortPossibles $
                            map toPossible $
                                filter (\(i,x) -> x == Nothing) $
                                    zip range puzzle
        -- puzzle solved
        getSolution [] puzzle range     = Just puzzle
        -- find solution
        getSolution (p:ps) puzzle range = testValues p (snd p) ps
            where
                -- no possibles -> no solution
                testValues p [] remaining     = Nothing
                -- find suitable solution at p = (index,[possibles])
                testValues p (x:xs) remaining =
                    if solution == Nothing
                        -- no solution -> next try
                        then testValues p xs remaining
                        else solution
                    where
                        -- index of empty field
                        at = fst p
                        -- update all remaining field according to test-value for p
                        updateRemaining x =
                            sortPossibles $
                                map (\(i,ps) ->
                                    if i `elem` indizes
                                        then (i,difference ps x)
                                        else (i,ps)) remaining
                            where
                                size = blockSize * blockSize
                                indizes =
                                    (getRow range at size) ++
                                    (getColumn range at size) ++
                                    (getBlock range at blockSize)
                        solution = getSolution
                            (updateRemaining [x])
                            ((take at puzzle) ++ [Just x] ++ (drop (at + 1) puzzle))
                            range        

{-
    get possible values for the given index
    according to its row, column and block
-}
getTestValues :: (Eq a) => [Maybe a] -> [a] -> Int -> Int -> [a]
getTestValues s base i blockSize = difference base combined
    where
        rowSize = blockSize*blockSize
        filterMaybe (Just x) = True
        filterMaybe Nothing  = False
        combined = map (\(Just x) -> x) $
                    filter filterMaybe $
                        union (getRow s i rowSize) $
                            union (getColumn s i rowSize) (getBlock s i blockSize)

-- utilizing function
parsePuzzle :: (Eq a) => [a] -> a -> [Maybe a]
parsePuzzle xs empty = map (\x -> if x == empty then Nothing else Just x) xs

getRow :: [a] -> Int -> Int -> [a]
getRow xs itemIndex rowSize = take rowSize $ drop (rowSize * rowIndex) xs
    where
        rowIndex = floor (fromIntegral itemIndex / fromIntegral rowSize)

buildColumn :: [a] -> Int -> Int -> [a]
buildColumn xs itemIndex rowSize
    | itemIndex >= (length xs) = []
    | otherwise                = xs!!itemIndex:buildColumn xs (itemIndex+rowSize) rowSize

getColumn :: [a] -> Int -> Int -> [a]
getColumn xs itemIndex rowSize
    | itemIndex >= rowSize = getColumn xs (itemIndex - rowSize) rowSize
    | otherwise            = buildColumn xs itemIndex rowSize

getBlock :: [a] -> Int -> Int -> [a]
getBlock xs itemIndex blockSize = buildBlock (blockSize-1)
    where
        rowSize = blockSize * blockSize
        y = floor $ (fromIntegral itemIndex) / (fromIntegral (rowSize * blockSize))
        x = (floor ((fromIntegral itemIndex) / (fromIntegral blockSize))) `mod` blockSize
        buildBlock index
            | index < 0 = []
            | otherwise = buildBlock (index - 1) ++
                            (take blockSize $
                                drop (x * blockSize)
                                    (getRow xs (y * rowSize * blockSize + rowSize * index) rowSize))

printSudoku :: (Show a) => Maybe [a] -> Int -> IO ()
printSudoku Nothing       s = print ""
printSudoku (Just [])     s = print ""
printSudoku (Just puzzle) s = do
    print (take s puzzle)
    printSudoku (Just $ drop s puzzle) s

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x `seq` (x : iterate' f (f x))

solvePuzzle puzzle = solveSudoku (parsePuzzle puzzle '-') "123456789" 3

iteratePuzzle n p =
    take n $ iterate' (\p -> case (solvePuzzle p) of {Nothing -> [] ; (Just ss) -> ss } ) p

puzzle0 =
    "------8-7" ++
    "3-8-42-65" ++
    "---1--932" ++
    "-6-75--9-" ++
    "95--3--21" ++
    "---4-9-5-" ++
    "417--3---" ++
    "59-814--3" ++
    "------4--"

puzzleVeryEasy0 =
    "-42761398" ++
    "--9543621" ++
    "-36298475" ++
    "9-468--53" ++
    "7-5439186" ++
    "6831752-9" ++
    "491357-6-" ++
    "3-89245-7" ++
    "2---16934"

puzzleEasy0 =
    "-3476--9-" ++
    "21---93-6" ++
    "97-3-8-54" ++
    "1829-6--3" ++
    "753-14--9" ++
    "-4-8-372-" ++
    "8-1-9-435" ++
    "3-748-612" ++
    "-65--29--"

puzzleMiddle0 =
    "6---43--9" ++
    "-92-67---" ++
    "3---5-4-8" ++
    "-6--3---2" ++
    "-5--1--4-" ++
    "1---8-65-" ++
    "7-15----6" ++
    "---4-6125" ++
    "---391--4"

puzzleHard0 =
    "32-------" ++
    "---7-5---" ++
    "-------6-" ++
    "5-1-----7" ++
    "----8--4-" ++
    "---6-----" ++
    "-8--3-7--" ++
    "------5-1" ++
    "------9--"

puzzleVeryHard0 =
    "5--4---2-" ++
    "3--8-6-1-" ++
    "--43--8--" ++
    "4--62----" ++
    "--6---7--" ++
    "----43--5" ++
    "--5--49--" ++
    "-7-2-8--1" ++
    "-4---7--8"

puzzleEmpty =
    replicate 81 '-'

puzzleTest0 =
    "-----1---" ++
    "-----3--1" ++
    "--6----7-" ++
    "9--------" ++
    "--5--9--6" ++
    "-8---5---" ++
    "4--3---6-" ++
    "------5--" ++
    "---------"

puzzle1 =
    [0,0,0,0,0,0,8,0,7] ++
    [3,0,8,0,4,2,0,6,5] ++
    [0,0,0,1,0,0,9,3,2] ++
    [0,6,0,7,5,0,0,9,0] ++
    [9,5,0,0,3,0,0,2,1] ++
    [0,0,0,4,0,9,0,5,0] ++
    [4,1,7,0,0,3,0,0,0] ++
    [5,9,0,8,1,4,0,0,3] ++
    [0,0,0,0,0,0,4,0,0]

main = do
    start <- getCurrentTime
    --let v = solveSudoku (parsePuzzle puzzleHard0 '-') "123456789" 3
    --let v = take 1000 $ iterate' (\p -> case (solvePuzzle p) of {Nothing -> [] ; (Just ss) -> ss } ) puzzle0
    let m = [1..20]
    let v = map (\x -> solveSudoku (parsePuzzle puzzleTest0 '-') "123456789" 3) m
    --let v = solveSudoku (parsePuzzle puzzleHard0 '-') "123456789" 3
    --printSudoku v 9
    stop <- v `deepseq` getCurrentTime
    print $ (show $ length m) ++ " runs:"
    --print v
    --stop <- getCurrentTime
    print $ diffUTCTime stop start