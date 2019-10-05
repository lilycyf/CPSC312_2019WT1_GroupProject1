-- CPSC 312 Project 1
-- Name: Yifei Chen
-- Student Number: 16394264
-- Name: 
-- Student Number: 
-- Name: 
-- Student Number: 

-- with the first integer as Value and second integer as position
data Sudoku = Sudoku [(Int, Int)]

-- solve :: Sudoku -> IO Sudoku or false
-- solve = solveloS

-- solveS S = if solved? S then S else solveloS (nextBoards S)

-- solveloS [] = 0
-- solveloS (h:t) = if ((solveS h) /= False) then (solveS h) else solveloS t

-- solved

-- nextBoards

-- find the place with no value
findBlanks :: Sudoku -> [Int]
findBlanks S = filter (notElem (position S)) [1..99]

-- find the place with value
position :: Sudoku -> [Int]
position S = map (\(v,p) -> p) S

-- fill position i with 1 to 9
fillWith1to9 :: Int -> Sudoku -> [Sudoku]
fillWith1to9 i S = [[(x,i):S] | x<-[1..9]]


-- pick a position to fill with a value and remove all the invalid Sudoku
keepOnlyValid :: Int -> [Sudoku] -> [Sudoku]
keepOnlyValid i [] = []
keepOnlyValid i (h:t) = if (validBoard i h) 
   then h: keepOnlyValid i t 
   else keepOnlyValid i t

-- check the cloum, row, and block that the position i is in
validBoard :: Int -> Sudoku -> Bool
validBoard i S = 




