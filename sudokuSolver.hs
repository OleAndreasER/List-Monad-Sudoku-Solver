import Data.List

main :: IO ()
main = do
    putStrLn "\nOriginal board: "
    putStrLn $ formatBoard board
    putStrLn "\nSolved board: "

type Board = [Row]
type Row = [Square] 
type Square = Char

board :: Board
board =
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


sudokuSolutions :: Board -> [Board]
sudokuSolutions = undefined

isLegalBoard :: Board -> Bool
isLegalBoard = undefined

legalBoards :: Board -> [Board]
legalBoards = predTolist isLegalBoard

type Coord = (Integer, Integer)

squareAtCoord :: Coord -> Board -> Square
squareAtCoord = undefined

placeSquare :: Coord -> Square -> Board -> Board
placeSquare = undefined

emptyCoords :: Board -> [Coord]
emptyCoords board =
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
