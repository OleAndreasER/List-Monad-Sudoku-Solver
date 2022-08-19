List Monad Sudoku Solver 
========================
The list monad (and applicative) can be used for non-deterministic computations. I wanted to use this to make a sudoku solver. It is not the most efficient probably, but that wasn't the point.

The program just prints to console. You can find the output in output.txt.

Non-deterministic functions take one input and may return multiple outputs (in a list):

```a -> [b]```

With list as a monad, we can apply these ```a -> [b]``` functions to ```[a]```. This gives ```[b]``` which contains all the return values from applying the function ```a -> [b]``` to all elements in ```[a]```.

This sudoku solver starts with a list of only one Board: The original ```[Board]```.

We have a function ```solveStep :: Board -> [Board]```. This returns all possible boards after one legal move.

Then we can apply this function to all these boards again and again until we have a solved board.

```[Board] -> [Board] -> [Board] -> etc.```

It looks like this:
```
[Board]
   |                                 =<< solveStep
   v------v------v
[Board, Board, Board]
   |             |                   =<< solveStep
   v             v------v------v
[Board,        Board, Board, Board]
etc.
```

I also used this for for finding all possible boards after one legal move.

We have the function:
```setSquare :: Board -> Square -> Coordinate -> Board```

Instead of using one ```Square``` and one ```Coordinate```, I instead supply ```[Square]``` and ```[Coordinate]```, giving me all possible combinations.

Here, ```[Square]``` is all possible Squares (1-9). ```[Coordinate]``` is all empty squares.

Finally I get rid of the illegal boards, and remove duplicates (to speed up the solver).

This uses list as an applicative.

```
solveStep :: Board -> [Board]
solveStep board =
    removeDuplicates
    $ legalBoards
    =<< setSquare board
    <$> squares
    <*> emptyCoords board
```
