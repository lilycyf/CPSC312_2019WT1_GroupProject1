-- CPSC 312 Project 1
-- Name: Yifei Chen
-- Student Number: 16394264
-- Name: Zhe Li
-- Student Number: 88792486
-- Name: Yixin Wang
-- Student Number: 36851582


module Main where

import Data.List

-- To run it, try:
-- main

-- with the first integer as value and second integer as position
type Sudoku = [(Integer, Integer)]


-- test case
testS = [(2,1),(7,2),(4,3),(9,5),(1,6),(5,9),
              (1,10),(5,13),(9,17),
              (6,19),(3,24),(2,25),(8,26),
              (1,30),(9,31),(8,36),
              (5,39),(1,40),(6,43),
              (7,46),(8,50),(3,54),
              (4,55),(2,57),(9,63),
              (7,71),
              (8,73),(3,76),(4,77),(9,78)]

-- a recusion body that enables user to type in the input and solve the sudoku
play s =
	if length s == 81
		then do
			putStrLn "The sudoku is solved:"
			render s
			putStrLn ""
                        askForSave s
		else do
			putStrLn "The current game is:"
			render s
			putStrLn ""
			askForSave s
			let oldSolution = sortBy sortGT (head (solve s))
			putStrLn "Do you need hint? (yes/no)"
			hint <- getLine
			if hint `elem` ["y","yes","ye","oui"]
				then play (giveHint oldSolution s)
				else do
					putStrLn "Do you want to see the solution? (yes/no)"
					cheat <- getLine
					if cheat `elem` ["y","yes","ye","oui"]
						then play oldSolution
						else do
							putStrLn "Input your sudoku movement position Natural[1, 81]"
							line1 <- getLine
							putStrLn "Input your sudoku movement value Natural[0, 9]"
							line2 <- getLine
							let posit = (read line1 :: Integer)
							let value = (read line2 :: Integer)
							if (checkValid value posit) == False
								then do
									putStrLn "Invalid input"
									play s
								else do
								let newS = makeMove (value, posit) s
								let newSolutions = solve newS
								if newSolutions == []
									then do
										putStrLn "There is no solution after this movement!"
										putStrLn "The movement has been reverted."
										play s
									else do
										play newS


-- check (value, position) is valid
checkValid :: Integer -> Integer -> Bool
checkValid v p = if ((p <= 0) || (p > 81))
                    then False
                    else
                      if ((v < 0) || (v > 9))
                        then False
                        else True


-- helper function to print the grids
renderS :: Sudoku -> Integer -> String

renderS s 81 =
      if length s == 0
          then "_"
          else show (fst (head s))

renderS [] n =
	if mod n 9 == 0
		then
      if mod n 27 == 0
        then
          "_" ++ "\n" ++ "======================" ++ "\n" ++ renderS [] (n+1)
        else
          "_" ++ "\n" ++ renderS [] (n+1)
		else
      if mod n 3 == 0
        then
          "_ | " ++ renderS [] (n+1)
        else do
          "_ " ++ renderS [] (n+1)

renderS (h:t) n
	| (snd h) == n 	= if mod n 9 == 0
						          then
                        if mod n 27 == 0
                          then
                            show (fst h) ++ "\n" ++ "======================" ++ "\n" ++ renderS t (n + 1)
                          else
                            show (fst h) ++ "\n" ++ renderS t (n + 1)
						          else do
                        if mod n 3 == 0
                          then
                            show (fst h) ++ " | " ++ renderS t (n + 1)
                          else
                            show (fst h) ++ " " ++ renderS t (n + 1)
	| otherwise 	= if mod n 9 == 0
						        then
                      if mod n 27 == 0
                        then
                          "_" ++ "\n" ++ "======================" ++ "\n" ++ renderS (h:t) (n + 1)
                        else
                          "_" ++ "\n" ++ renderS (h:t) (n + 1)
						        else
                      if mod n 3 == 0
                        then
                          "_ | " ++ renderS (h:t) (n + 1)
                        else
                          "_ " ++ renderS (h:t) (n + 1)

render :: Sudoku -> IO()
render s = putStr(renderS s 1)

myprint :: Sudoku -> String
myprint s = renderS s 1

askForSave :: Sudoku -> IO()
askForSave s = do
                putStrLn "Do you want to save? (yes/no)"
                saveV <- getLine
                if saveV `elem` ["y","yes","ye","oui"]
                  then do
                    putStrLn "Please enter the filename you want it to save to:"
                    filename <-getLine
                    writeFile (filename ++ ".txt") (myprint s)
                  else do
                    putStr ""

-- give a hint that is the first empty cell in the present sudoku according to the solution
giveHint:: Sudoku -> Sudoku -> Sudoku
giveHint soln [] = [head soln]
giveHint (h1:t1) (h2:t2)
	| (snd h1) == (snd h2)	= h2 : giveHint t1 t2
	| otherwise 			= h1 : h2 : t2

-- put the pair input given by user to the correct place in sudoku
makeMove:: (Integer, Integer) -> Sudoku -> Sudoku
makeMove (v, p) [] = [(v, p)]
makeMove (v, p) (h:t)
	| p > (snd h) 	= h : makeMove (v, p) t
	| otherwise 	= (v, p) : h : t

solve s = if initialCheck s then solveS s else []

initialCheck :: [(Integer, Integer)] -> Bool
initialCheck s = foldr (\ x y -> if validBoard x s then True && y else False && y) True (position s)

solveS :: [(Integer, Integer)] -> [[(Integer, Integer)]]
solveS s = if solved s then [s] else solveloS (nextBoards s)

solveloS :: [[(Integer, Integer)]] -> [[(Integer, Integer)]]
solveloS los = foldr (\ x y -> (solveS x)++y) [] los

-- produce true if board is solved, it is solved if it is full
solved :: [(Integer, Integer)] -> Bool
solved s = if (findBlanks s) == [] then True else False


-- produce list of valid next boards from board
nextBoards :: [(Integer, Integer)] -> [[(Integer, Integer)]]
nextBoards s = (keepOnlyValid (head (findBlanks s)) (fillWith1to9 (head (findBlanks s)) s))

-- find the place with no value
findBlanks :: [(Integer, Integer)] -> [Integer]
findBlanks s = foldr (\ x y -> if notElem x (position s) then x:y else y) [] [1..81]

-- get all positions
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
validBoard i s = (valid sameColum i s) && (valid sameRow i s) && (valid sameBlock i s)

valid :: (Integer -> [(Integer, Integer)] -> [(Integer, Integer)]) -> Integer -> [(Integer, Integer)] -> Bool
valid f i s = if (length (foldr (\ v y -> if notElem v y then v:y else y) [] (value (f i s)))) == length (f i s)
   then True else False

sameColum :: Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
sameColum i s = foldr (\ (v,p) y -> if (elem p [i, i-9..0])||(elem p [i, i+9..81]) then (v,p):y else y) [] s

sameRow :: Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
sameRow i s = foldr (\ (v,p) y -> if (((i - (mod i 9))< p) && (p <=(i + (9 - (mod i 9))))) then (v,p):y else y) [] s

sameBlock :: Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
sameBlock i s = foldr (\ (v,p) y -> if elem p (sameBlockHelper i) then (v,p):y else y) [] s

sameBlockHelper :: Integer -> [Integer]
sameBlockHelper i
   | elem i [1,2,3,10,11,12,19,20,21] = [1,2,3,10,11,12,19,20,21]
   | elem i [4,5,6,13,14,15,22,23,24] = [4,5,6,13,14,15,22,23,24]
   | elem i [7,8,9,16,17,18,25,26,27] = [7,8,9,16,17,18,25,26,27]
   | elem i [28,29,30,37,38,39,46,47,48] = [28,29,30,37,38,39,46,47,48]
   | elem i [31,32,33,40,41,42,49,50,51] = [31,32,33,40,41,42,49,50,51]
   | elem i [34,35,36,43,44,45,52,53,54] = [34,35,36,43,44,45,52,53,54]
   | elem i [55,56,57,64,65,66,73,74,75] = [55,56,57,64,65,66,73,74,75]
   | elem i [58,59,60,67,68,69,76,77,78] = [58,59,60,67,68,69,76,77,78]
   | elem i [61,62,63,70,71,72,79,80,81] = [61,62,63,70,71,72,79,80,81]


-- get all values
value :: [(Integer, Integer)] -> [Integer]
value s = map (\(v,p) -> v) s


-- a sort helper that put the pair of integer in increasing order in sudoku
sortGT (av, ap) (bv, bp)
	| ap < bp 	= LT
	| otherwise = GT


-- read input from the user
myRead s n =
	if n < 17
		then do
			putStrLn "Input your sudoku movement position Natural[1, 81]"
			line1 <- getLine
			putStrLn "Input your sudoku movement value Natural[0, 9]"
			line2 <- getLine
			let posit = (read line1 :: Integer)
			let value = (read line2 :: Integer)
			let newS = makeMove (value, posit) s
			if (checkValid value posit) == False
				then do
					putStrLn "Invalid input"
					myRead s n
				else 
					myRead newS (n + 1)
		else play s


-- initial state: [], 0
main = do
	putStrLn "Please input your sudoku game!"
	myRead [] 0
