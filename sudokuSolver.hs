import Data.List
import Data.Char (digitToInt)
import Data.Set (toList, fromList)

main :: IO ()
main = mapM_ solve [board1, board2]

solve :: Board -> IO ()
solve board = do
    putStrLn "\nOriginal board:"
    putStrLn $ formatBoard board
    putStrLn "\nSolution:"
    putStrLn $ formatBoard $ solveBoard board

type Board = [Row]
type Row = [Square] 
type Square = Char

board1 :: Board
board1 =
    [ "801249000"
    , "000506000"
    , "002030809"
    , "000020030"
    , "435008060"
    , "086014795"
    , "014002683"
    , "703050924"
    , "920060517"
    ]

board2 :: Board
board2 =
    [ "276800300"
    , "834756092"
    , "159420867"
    , "903547086"
    , "685231749"
    , "420698503"
    , "540362900"
    , "702084035"
    , "368970420"
    ]

solveBoard :: Board -> Board
solveBoard board = head $ iterateUntil fullBoard solveStep [board]

solveStep :: Board -> [Board]
solveStep board =
    removeDuplicates
    $ legalBoards
    =<< setSquare board
    <$> squares
    <*> emptyCoordinates board

iterateUntil :: Monad m => (a -> Bool) -> (a -> m a) -> m a -> m a
iterateUntil pred f x = do
    y <- f =<< x
    if pred y
    then pure y
    else iterateUntil pred f $ pure y

fullBoard :: Board -> Bool
fullBoard = all (all (/= '0'))

legalBoards :: Board -> [Board]
legalBoards = predTolist isLegalBoard

isLegalBoard :: Board -> Bool
isLegalBoard board =
    all (all isLegalSquares)
    [ rows board
    , columns board
    , groups board
    ]

rows :: Board -> [[Square]]
rows = id

columns :: Board -> [[Square]]
columns = transpose 

groups :: Board -> [[Square]]
groups board =
    [[board !! (y+(y'*3)) !! (x+(x'*3))
     | y <- [0..2], x <- [0..2]
     ]
    | y' <- [0..2], x' <- [0..2]
    ]

isLegalSquares :: [Square] -> Bool
isLegalSquares squares =
    all ((< 2) . length)
    $ group
    $ sort
    $ digitToInt
    <$> filter (/= '0') squares

type Coordinate = (Integer, Integer)

squareAtCoordinate :: Coordinate -> Board -> Square
squareAtCoordinate (row,square) board = board !! fromIntegral row !! fromIntegral square 

setSquare :: Board -> Square -> Coordinate -> Board
setSquare board c (row,square) =
    setAt (fromIntegral row) newRow board
  where
    newRow = setAt (fromIntegral square) c $ board !! fromIntegral row 

setAt :: Int -> a -> [a] -> [a]
setAt i x xs = left ++ [x] ++ right
    where (left, _ : right) = splitAt i xs

emptyCoordinates :: Board -> [Coordinate]
emptyCoordinates board =
    [ (n,m)
    | (n, squares) <- enumerateBoard board
    , (m, square)  <- squares
    , square == '0'
    ]

enumerateBoard :: Board -> [(Integer, [(Integer, Square)])]
enumerateBoard board = zip [0..] $ map (zip [0..]) board

predTolist :: (a -> Bool) -> a -> [a]
predTolist pred x = if pred x then [x] else []

squares :: [Square]
squares = ['1', '2', '3', '4', '5', '6', '7', '8', '9']

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = toList . fromList

--Formatting
formatBoard :: Board -> String
formatBoard = unlines . map formatRow

formatRow :: Row -> String
formatRow row = 
    (" | " ++)
    $ (: " | ")
    =<< emptySquare
    <$> row

emptySquare :: Char -> Square
emptySquare '0' = ' '
emptySquare s   = s
