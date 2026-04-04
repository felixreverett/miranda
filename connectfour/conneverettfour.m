|| A simulation of connect four.
|| Takes a list of moves, alternating turns between two teams
|| marked with X and O. Players may select a number within
|| the range of the board, outside of which the move is
|| invalid.

|| ========================================================== ||
|| Game types below:                                          ||
|| ========================================================== ||

|| This moveslist should result in the second player winning.
|| [char] has been chosen as the datatype over char because
|| this more correctly maps to the input format from a
|| command-line version of the game. 

movesList
  = ["3", "3", "2", "4",
     "1", "0", "2", "2",
     "3", "1", "3", "2",
     "2", "1"]

defaultBoard
  = ["_______",
     "_______",
     "_______",
     "_______",
     "_______",
     "_______"
    ]

|| The 'gameState' is calculated after every move of the game
|| to determine if a 'player' has won the game. The player
|| is represented by a char collected by determining who made
|| the most recent move. While this data can be collected
|| separately to the gameState, I have chosen to include it
|| within the algebraic type to improve readability of the
|| code.

gameState ::= Winner char | Stalemate | Ongoing

|| ========================================================== ||

main = run

|| ========================================================== ||

run
  = playAllMoves movesList
  || printBoard (updateBoard defaultBoard 3 3 'X')

|| ========================================================== ||

playAllMoves :: [[char]] -> [char]
playAllMoves [] = error "please enter a move"
playAllMoves (x : xs) = playMove x

|| ========================================================== ||

|| processUserInput takes a user input and

processUserInput
  = error "not implemented exception"

|| ========================================================== ||

|| switch user is a simple function to be called after each
|| turn to determine which user will have their move played.

switchUser :: char -> char
switchUser 'X' = 'O'
switchUser 'O' = 'X'
switchUser any = error "Unknown user. How did we get here?"

|| ========================================================== ||

|| isGameWon is called after every turn to see if the win
|| condition has

isGameWon :: [[char]] -> gameState
isGameWon board

|| ========================================================== ||

|| updateBoard takes a pre-validated input and piece to play
|| ('X' or 'O'), and returns an updated board with the tile
|| at those coordinates filled in. Since the user only needs
|| to select a column, the selection of the row is
|| pre-processed outside of this function, allowing for the
|| function to not require input validate.

updateBoard :: [[char]] -> num -> num -> char -> [[char]]
updateBoard board row col value
  = (take row board)
    ++ [take col (board!row)
       ++ [value] ++ drop (col+1) (board!row)]
    ++ drop (row+1) board

|| ========================================================== ||

|| printBoard is a simple function which takes a connect four
|| board and prints it.

printBoard :: [[char]] -> [char]
printBoard board
  = lay board