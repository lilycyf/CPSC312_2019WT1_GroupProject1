-- CPSC 312 Project 1
-- Name: Yifei Chen
-- Student Number: 16394264
-- Name:
-- Student Number:
-- Name: Yixin Wang
-- Student Number: 36851582


import GI.GTK


data SudokuUI = SudokuUI {window :: Window,
                          menu   :: GameMenu,
                          cell   :: Cells}


-- write cell to the board
writeCell :: Cell -> Sudoku -> IO
