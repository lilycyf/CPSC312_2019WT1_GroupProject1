module Main where


type Sudoku = [(Integer, Integer)]


-- initial state: [], 0
main s n = 
	if n < 17
		then do
			putStrLn "Input your sudoku movement position Natural[1, 81]"
			line1 <- getLine
			putStr "Input your sudoku movement value Natural[0, 9]"
			line2 <- getLine
			let posit = (read line1 :: Int)
			let value = (read line2 :: Int)
			let newS = makeMove (value, posit) s
			main newS (n + 1)
		else play s


play s =
	if length s == 81
		then do
			putStrLn "The sudoku is solved:"
			render s
		else do
			putStrLn "The current game is:"
			render s

			let oldSolution = head solve s

			putStrLn "Do you need hint? (true/false)"
			line3 <- getLine
			let hint = (read line3 :: Bool)
			if hint
				then play (giveHint oldSolution s)
				else do
					putStrLn "Do you want to see the solution? (true/false)"
					line4 <- getLine
					let cheat = (read line4 :: Bool)
					if cheat
						then play solution
						else 
							putStrLn "Input your sudoku movement position Natural[1, 81]"
							line1 <- getLine
							putStr "Input your sudoku movement value Natural[0, 9]"
							line2 <- getLine
							let posit = (read line1 :: Int)
							let value = (read line2 :: Int)
							let newS = makeMove (value, posit) s
							let newSolutions = solve newS
							if newSolutions == []
								then do
									putStrLn "There is no solution after this movement!"
									putStrLn "The movement has been reverted."
									play s
								else do
									play newS


render:: Sudoku -> [Char]
render s = [] -- stub


giveHint:: Sudoku -> Sudoku -> Sudoku
giveHint s s = s -- stub 


makeMove:: (Int, Int) -> Sudoku -> Sudoku
makeMove (v, p) s = s -- stub

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
