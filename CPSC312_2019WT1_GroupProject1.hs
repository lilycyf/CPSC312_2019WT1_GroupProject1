-- CPSC 312 Project 1
-- Name: Yifei Chen
-- Student Number: 16394264
-- Name: 
-- Student Number: 
-- Name: 
-- Student Number: 

data Sudoku = Sudoku [(int,int)]
data Sudoku = Sudoku [int]

solve :: Sudoku -> IO Sudoku or false
solve = solveloS

solveS S = if solved? S then S else solveloS (nextBoards S)

solveloS [] = 0
solveloS (h:t) = if ((solveS h) /= False) then (solveS h) else solveloS t

solved?

nextBoards

fillWith1to9

keepOnlyValid

ReadSquare

FillSquare S p i = ()


