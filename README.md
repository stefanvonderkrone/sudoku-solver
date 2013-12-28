# sudoku-solver

Sudoku-Solver made in several programming languages

## The Algorithm

### Backtracking

The first version `sudoku.**` uses a simple recursive backtracking algorithm, where the program iterates over each field of the sudoku (the puzzle must be one-dimensional). With each field, a solution-list will be extended. If the new field already has a value, the solution to this field is found and added to the solution-list. If there is an empty field, it will be examined to determine its possible solutions. Each solution will be tested until there is a complete puzzle solution. If not, there is no solution to the sudoku.

### Backtracking with Sorting

The second version `sudoku2.**` uses a recursive backtracking algorithm with sorting. First, all empty fields will be filtered and sorted ascending according to their number of possible solutions. If some fields only have one possible value, their solution is found. (This can be a huge speed up.) Next, the program iterates over the filtered list and combines their first solution with the puzzle to find the overall solution.

## The Languages

### Javascript

    $ node sudoku.js
    
    $ node sudoku2.js

The second version for javascript has a bug which leeds to no-solution `puzzleHard0`.

### Haskell

    $ runhaskell sudoku.hs
    $ ghci sudoku.hs
    $ ghc sudoku.hs && ./sudoku
    
    $ runhaskell sudoku2.hs
    $ ghci sudoku2.hs
    $ ghc sudoku2.hs && ./sudoku2
