|| A simulation of connect four.
||
|| Takes a list of moves and processes them as alternating
|| turns between two players marked as X and O. Players may
|| select a number within the range of the board, outside of
|| which the move is invalid.
||
|| For the purpose of demonstration, the game does not display
|| the board until and end condition has been reached.

|| ========================================================== ||
|| Game datatypes below:                                      ||
|| ========================================================== ||

|| This moveslist should result in the second player winning.
|| [char] has been chosen as the datatype over char because
|| this more correctly maps to the input format that a
|| command-line version of the game would have.

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
|| the most recent move.

gameState ::= Winner | Stalemate | Ongoing

|| ========================================================== ||

main = run

|| ========================================================== ||

run
  = playAllMoves movesList defaultBoard

|| ========================================================== ||

|| playAllMoves processes every move and returns once either
|| all moves are exhausted (wouldn't happen in actual game)
|| or an outcome is found.

|| The helper function xPAM is used to initialise the game
|| with common starter states, such that it can be abstracted
|| from the primary playAllMoves function.

playAllMoves :: [[char]] -> [char]

playAllMoves moves board
  = xPAM moves (board, Ongoing, 'X') || initial state
    where
    xPAM []       any
      = "Run out of moves"
    xPAM any      (b, Stalemate, p)
      = error "Stalemate not implemented exception"
    xPAM any      (b, Winner, p)
      = error "Winner not implemented exception"
    xPAM (m : ms) (b, gs, p)
      = xPAM ms (playMove b m p)
    xPAM any other
      = error "Invalid game state passed into xPAM"

|| ========================================================== ||

|| playMove takes a board and move returns a board with its
|| new game state. The decision to include the gameState at
|| this part of the evaluation has been made to streamline
|| the calling of playMove within xPAM above.

playMove :: [[char]] -> [char] -> char      || in
            -> ([[char]], gameState, char)  || out

playMove board move player
  = (nextBoard, nextGameState, nextPlayer)
    where
    parsedMove    = parseMove move
    nextBoard     = updateBoard board parsedMove player
    nextGameState = getGameState newBoard
    nextPlayer    = getNextPlayer player

|| ========================================================== ||

|| parseMove takes a user input and attempts to turn it into
|| into a number

parseMove :: [char] -> num

parseMove m
  = parsedValidatedMove
    where
    parsedMove = numval m
    parsedValidatedMove
      = parsedMove, if parsedMove > 0 & parsedMove < 7
      = error "Out of bounds move played", otherwise

|| ========================================================== ||

|| getNextPlayer is a simple function to be called after each
|| turn to determine which user will have their move played.

getNextPlayer :: char -> char
getNextPlayer 'X' = 'O'
getNextPlayer 'O' = 'X'
getNextPlayer any = error "Unknown user. How did we get here?"

|| ========================================================== ||

|| getGameState is called after every turn to see if the win
|| or stalemate condition has been met

getGameState :: [[char]] -> gameState
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