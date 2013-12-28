import Data.List
import Data.Maybe
import Data.Time
import Control.DeepSeq

difference base xs = base \\ xs

solveSudoku :: (Eq a) => [Maybe a] -> [a] -> Int -> Maybe [a]
solveSudoku puzzle base blockSize = getSolution [] puzzle
    where
        getSolution valids []     = Just valids
        getSolution valids (x:xs) =
            case x of Nothing -> testValues $
                                    getTestValues
                                        ((toMaybes valids) ++ (x:xs))
                                        base
                                        (length valids)
                                        blockSize
                      Just y  -> getSolution (valids ++[y]) xs
                where
                    toMaybes [] = []
                    toMaybes xs = map (\x -> Just x) xs
                    testValues []     = Nothing
                    testValues (p:ps) =
                        if solution == Nothing then (testValues ps) else solution
                        where
                            solution = getSolution (valids ++ [p]) xs

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
    --let v = iteratePuzzle 1000 puzzleEmpty
    --printSudoku v 9
    let m = [1..20]
    let v = map (\x -> solveSudoku (parsePuzzle puzzleHard0 '-') "123456789" 3) m
    stop <- v `deepseq` getCurrentTime
    --print v
    --stop <- getCurrentTime
    print $ (show $ length m) ++ " runs:"
    print $ diffUTCTime stop start