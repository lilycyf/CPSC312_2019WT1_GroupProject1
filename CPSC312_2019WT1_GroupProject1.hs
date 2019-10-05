-- CPSC 312 Project 1
-- Name: Yifei Chen
-- Student Number: 16394264
-- Name: 
-- Student Number: 
-- Name: 
-- Student Number: 

-- with the first integer as Value and second integer as position
data Sudoku = Sudoku [(Integer, Integer)]

testSudoku = [(1,2)]

-- solve :: Sudoku -> IO Sudoku or false
-- solve = solveloS

-- solveS S = if solved? S then S else solveloS (nextBoards S)

-- solveloS [] = 0
-- solveloS (h:t) = if ((solveS h) /= False) then (solveS h) else solveloS t

-- solved

-- nextBoards

-- find the place with no value
findBlanks :: [(Integer, Integer)] -> [Integer]
findBlanks s = foldr (\ x y -> if notElem x (position s) then x:y else y) [] [1..81]

-- find the place with value
position :: [(Integer, Integer)] -> [Integer]
position s = map (\(v,p) -> p) s

-- fill position i with 1 to 9
fillWith1to9 :: Integer -> [(Integer, Integer)] -> [[(Integer, Integer)]]
fillWith1to9 i s = [(x,i):s | x<-[1..9]]


-- pick a position to fill with a value and remove all the invalid Sudoku
keepOnlyValid :: Integer -> [[(Integer, Integer)]] -> [[(Integer, Integer)]]
keepOnlyValid i [] = []
keepOnlyValid i (h:t) = if (validBoard i h) 
   then h: keepOnlyValid i t 
   else keepOnlyValid i t

-- check the cloum, row, and block that the position i is in
validBoard :: Integer -> [(Integer, Integer)] -> Bool
validBoard i s = True




