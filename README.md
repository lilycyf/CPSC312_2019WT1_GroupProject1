# CPSC312_2019WT1_GroupProject1: Sudoku #

## Introduction

Sudoku is a logic-based, combinatorial number-placement puzzle.
The objective is to fill a 9×9 grid with digits so that each column, each row, and each of the nine 3×3 subgrids that compose the grid contain all of the digits from 1 to 9.

This project intends to construct a Sudoku solver to help player solve the sudoko game. The player can construct and fill in a sudoku by repetitively putting numbers to certain coordinator. The solver consumes the input from player, and computes the solution. Player can ask the computer for hints or solution.

The smallest sudoku that can be solved with one possible solution has 17 known numbers. Therefore, we ask the player to give the solver at least 17 numbers as the input. Then the solver will solve the sudoku and player can choose whether he/she needs a hint or solution.
A hint is to give the first empty cell in the present sudoku while a solution is present the solution computed by the solver directed on the screen. Player can do the sudoku and give his/her intermediate step to the solver to check if he/she is on the right track, which means if that number is placed on that position, then the solver will check whether there is one possible solution. When there is no solution, the solver will report to the player that the game is not solvable.
We will give player the solution of each step by clearing printing it on the terminal.

-------------------------------------------------

## Contributors

Name: Yifei Chen

Name: Zhe Li

Name: Yixin Wang
