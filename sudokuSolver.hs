import Data.List

main = do
    putStrLn "\nOriginal board: "
    putStrLn $ formatSudokuBoard board
    putStrLn "\nSolved board: "
    putStrLn . formatSudokuBoard $ solveSudoku board

board = [
    "801249000",
    "000506000",
    "002030809",
    "000020030",
    "435008060",
    "086014795",
    "014002683",
    "703050924",
    "920060517"]

formatSudokuRow row = 

emptySquare :: 
emptySquare '0' = " "
emptySquare s   = s

formatSudokuBoard = map formatSudokuRow

solveSudoku board = board
