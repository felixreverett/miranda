|| A simulation of connect four.
|| Takes a list of moves, alternating turns between two teams
|| marked with X and O. Players may select a number within
|| the range of the board, outside of which the move is
|| invalid.

movesList = ["4", "3", "2"]

defaultBoard
  = ["_______",
     "_______",
     "_______",
     "_______",
     "_______",
     "_______"
    ]

|| gameOutcome is used 
gameOutcome ::= Winner | Stalemate | Ongoing

|| ========================================================== ||

main = run

|| ========================================================== ||

run = printBoard (updateBoard defaultBoard 3 3 'X')

|| ========================================================== ||

|| processUserInput takes a user input and

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

isGameWon :: [[char]] -> gameOutcome
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