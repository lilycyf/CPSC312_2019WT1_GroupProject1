-- CPSC 312 Project 1
-- Name: Yifei Chen
-- Student Number: 16394264
-- Name:
-- Student Number:
-- Name: Yixin Wang
-- Student Number: 36851582

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk

data SudokuUI = SudokuUI {window :: Window}

data Board = Board {board :: [[Maybe Int]]} deriving(Show)

main :: IO()
main = do
        void initGUI
        window <- windowNew
        widgetShowAll window
        mainGUI

emptyBoard :: Board
emptyBoard = Board[[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
                  [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
                  [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
                  [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
                  [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
                  [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
                  [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
                  [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
                  [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]]


-- write cell to the board
--writeCell :: Board -> Int -> Int -> Int -> Board
--writeCell board row col val =
