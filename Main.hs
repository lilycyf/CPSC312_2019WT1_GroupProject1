module Main where

main s = do
	putStrLn "The current game is:"
	render s
	putStrLn "Input your sudoku movement position Natural[1, 81]"
	line1 <- getLine
	putStr "Input your sudoku movement value Natural[0, 9]"
	line2 <- getLine
	let posit = (read line1 :: Int)
	let value = (read line2 :: Int)
	let solution = solve (value, posit):s
	if solution == []
		then do 
			putStrLn "There is no solution after this movement!"
			putStrLn "The movement has been reverted."
			main s
		else do
			putStrLn "Good movement!"
			putStrLn "Do you need hint? (true/false)"
			line3 <- getLine
			let hint = (read line3 :: Bool)
			if hint
				then main (giveHint solution s) : s
				else "No hint is needed."
			putStrLn "Do you want to see the solution? (true/false)"
			line 4 <- getLine
			let cheat = (read line4 :: Bool)
			if cheat
				then render solution
				else main (value, posit):s






