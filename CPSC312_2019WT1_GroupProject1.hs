-- CPSC 312 Project 1
-- Name: Yifei Chen
-- Student Number: 16394264
-- Name: Zhe Li
-- Student Number: 88792486
-- Name: Yixin Wang
-- Student Number: 36851582


-- To run it, try:
-- solve testSudoku1

-- with the first integer as Value and second integer as position
type Sudoku = [(Integer, Integer)]

testSudoku = [(2,1),(7,2),(4,3),(8,4),(9,5),(1,6),(3,7),(6,8),(5,9),
              (1,10),(3,11),(8,12),(5,13),(2,14),(6,15),(4,16),(9,17),(7,18),
              (6,19),(5,20),(9,21),(4,22),(7,23),(3,24),(2,25),(8,26),(1,27),
              (3,28),(2,29),(1,30),(9,31),(6,32),(4,33),(7,34),(5,35),(8,36),
              (9,37),(8,38),(5,39),(1,40),(3,41),(7,42),(6,43),(4,44),(2,45),
              (7,46),(4,47),(6,48),(2,49),(8,50),(5,51),(9,52),(1,53),(3,54),
              (4,55),(6,56),(2,57),(7,58),(5,59),(8,60),(1,61),(3,62),(9,63),
              (5,64),(9,65),(3,66),(6,67),(1,68),(2,69),(8,70),(7,71),(4,72),
              (8,73),(1,74),(7,75),(3,76),(4,77),(9,78),(5,79),(2,80),(6,81)]


-- Four possiable solutions
testSudoku1 = [(2,1),(7,2),(4,3),(9,5),(1,6),(5,9),
              (1,10),(5,13),(9,17),
              (6,19),(3,24),(2,25),(8,26),
              (1,30),(9,31),(8,36),
              (5,39),(1,40),(6,43),
              (7,46),(8,50),(3,54),
              (4,55),(2,57),(9,63),
              (7,71),
              (8,73),(3,76),(4,77),(9,78)]

-- No sulution: Fail initialCheck
testSudoku2 = [(1,1),(1,2),(1,3),(8,4),(9,5),(1,6),(3,7),(6,8),(5,9),
              (1,10),(1,11),(1,12),(5,13),(2,14),(6,15),(4,16),(9,17),(7,18),
              (1,19),(1,20),(1,21),(4,22),(7,23),(3,24),(2,25),(8,26),(1,27),
              (3,28),(2,29),(1,30),(9,31),(6,32),(4,33),(7,34),(5,35),(8,36),
              (9,37),(8,38),(5,39),(1,40),(3,41),(7,42),(6,43),(4,44),(2,45),
              (7,46),(4,47),(6,48),(2,49),(8,50),(5,51),(9,52),(1,53),(3,54),
              (4,55),(6,56),(2,57),(7,58),(5,59),(8,60),(1,61),(3,62),(9,63),
              (5,64),(9,65),(3,66),(6,67),(1,68),(2,69),(8,70),(7,71),(4,72),
              (8,73),(1,74),(7,75),(3,76),(4,77),(9,78),(5,79),(6,81)]

-- One possiable solution
testSudoku3 = [(2,1),(7,2),(4,3),(8,4),(9,5),(1,6),(3,7),(6,8),(5,9),
              (1,10),(3,11),(8,12),(5,13),(2,14),(6,15),(4,16),(9,17),
              (6,19),(5,20),(9,21),(4,22),(7,23),(3,24),(2,25),(8,26),(1,27),
              (3,28),(2,29),(1,30),(9,31),(6,32),(4,33),(7,34),(5,35),(8,36),
              (9,37),(8,38),(5,39),(1,40),(3,41),(7,42),(6,43),(4,44),(2,45),
              (7,46),(4,47),(6,48),(2,49),(8,50),(5,51),(9,52),(1,53),(3,54),
              (4,55),(6,56),(2,57),(7,58),(5,59),(8,60),(1,61),(3,62),(9,63),
              (5,64),(9,65),(3,66),(6,67),(1,68),(2,69),(8,70),(7,71),(4,72),
              (8,73),(1,74),(7,75),(3,76),(4,77),(9,78),(5,79)]




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
