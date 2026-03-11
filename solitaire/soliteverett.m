|| ==========================================================================================================
||   /$$$$$$            /$$ /$$   /$$                                                         /$$     /$$    
||  /$$__  $$          | $$|__/  | $$                                                        | $$    | $$    
|| | $$  \__/  /$$$$$$ | $$ /$$ /$$$$$$    /$$$$$$  /$$    /$$ /$$$$$$   /$$$$$$   /$$$$$$  /$$$$$$ /$$$$$$  
|| |  $$$$$$  /$$__  $$| $$| $$|_  $$_/   /$$__  $$|  $$  /$$//$$__  $$ /$$__  $$ /$$__  $$|_  $$_/|_  $$_/  
||  \____  $$| $$  \ $$| $$| $$  | $$    | $$$$$$$$ \  $$/$$/| $$$$$$$$| $$  \__/| $$$$$$$$  | $$    | $$    
||  /$$  \ $$| $$  | $$| $$| $$  | $$ /$$| $$_____/  \  $$$/ | $$_____/| $$      | $$_____/  | $$ /$$| $$ /$$
|| |  $$$$$$/|  $$$$$$/| $$| $$  |  $$$$/|  $$$$$$$   \  $/  |  $$$$$$$| $$      |  $$$$$$$  |  $$$$/|  $$$$/
||  \______/  \______/ |__/|__/   \___/   \_______/    \_/    \_______/|__/       \_______/   \___/   \___/  
|| ==========================================================================================================



|| ======================================================================================
|| This is the Felix implementation of Solitaire, also known as 'Soliteverett'
||
|| Major Changes:
|| - Optimised logic in checkmove and makemove to use directional tuples instead;
|| - Added dynamic boardsizing. This requires that the array not be jagged;
|| - You can play diagonally;
|| - Added a score to the accumulator. Yes you could just type `# moves`,
||   but the score is incremented in the accumulator;
|| - Added file loading and parsing of text into a pegboard;
|| - The game is now interactive!
||
|| Fixes:
|| - The initial implementation did not prevent off-board moves East or West;
|| - Added explicit error checking to ensure moves cannot land on an invalid tile,
||   which was previously handled incidentally by ensuring the destination was a Hole.
||
|| ======================================================================================

|| The pegstate type represents the three possible states of each cell on a board.
|| Invalid signifies an "out-of-bounds" cell to allow for non-rectangular boards.

pegstate ::= Peg | Hole | Invalid

|| These values denote unit movement offsets to simplify movement logic

north     = ( 0, -1)
east      = ( 1,  0)
south     = ( 0,  1)
west      = (-1,  0)
northeast = ( 1, -1)
southeast = ( 1,  1)
southwest = (-1,  1)
northwest = (-1, -1)

|| Default board state. This can be modified.

pegboard == [[pegstate]]

defaultBoard
  = [
      [Invalid, Invalid, Peg, Peg,  Peg, Invalid, Invalid],
      [Invalid, Invalid, Peg, Peg,  Peg, Invalid, Invalid],
      [Peg,     Peg,     Peg, Peg,  Peg, Peg,     Peg],
      [Peg,     Peg,     Peg, Hole, Peg, Peg,     Peg],
      [Peg,     Peg,     Peg, Peg,  Peg, Peg,     Peg],
      [Invalid, Invalid, Peg, Peg,  Peg, Invalid, Invalid],
      [Invalid, Invalid, Peg, Peg,  Peg, Invalid, Invalid]
    ]

|| movement defines an [x,y] coordinate, and the direction of movement as defined above.

movement == (num, num, (num, num))

|| ======================================================================================
|| fn printBoard
|| Takes a board and score of type num, and returns a string
|| ======================================================================================

printBoard :: (pegboard, num) -> [char]
printBoard (b, score)
      = divider ++ spacer ++ (lay (map (map g) b)) ++ spacer ++ divider
        ++ "\nScore: " ++ show (score + 0) ++ spacer ++ divider
        where
        g Peg = 'O'
        g Invalid = ' '
        g any = '_'
        divider = ['=' | i <- [1..#(b!0)]]
        spacer = "\n"

|| ======================================================================================
|| fn loadBoard
|| loads a board from a file given the filename.
||
|| NB: entering an invalid filename will crash the program.
|| ======================================================================================

loadBoard :: [char] -> [[pegstate]]
loadBoard fileName
  = (parseBoard.loadFile) fileName
    where

    || simple step to read file
    loadFile fName = filter (~= '\r') (read fileName)

    || parseBoard [char] -> [[pegstate]]
    || > splits fileContent by lines
    || > then splits those lines into rows by whitespace
    || > finally maps each element of each row to its corresponding pegstate

    parseBoard fileContent = ( (map parseRow).(map (split ' ')).lines) fileContent
    parseRow boardRow = map strToPegState boardRow
    strToPegState "I" = Invalid
    strToPegState "P" = Peg
    strToPegState "H" = Hole
    strToPegState any = error ("Unrecognised pegstate found in " ++ show (fileName) ++ ": " ++ show (any))

|| ======================================================================================
|| fn calculateGameOver
|| - Returns True if no more valid moves are possible
|| - Iterates over every row/col to see if a valid move can be made at that location.
||
|| Possible bug: A winning board will be considered Game Over, which is correct but
|| this should probably log something else to the user.
|| ======================================================================================

calculateGameOver :: pegboard -> bool
calculateGameOver board

  || this logic uses List Comprehensions to generate all coordinates of the board
  || passing each set into 'canMoveHere' and xor-ing the result
  || this means the function returns early if a valid move is found

  = ~ (or [canMoveHere (r, c, board) | r <- [0 .. #board - 1]; c <- [0 .. #(board!0) - 1]])
    where
    
    || canMoveHere :: (num, num, pegboard) -> bool
    ||   this function checks each cell to see if it's a Peg
    ||   then checks adjacent cells in 8 directions using 'checkDir' to see
    ||   if they contain a Peg to jump over, and a Hole to land in.
    ||   This function returns early if a valid move is found.
    
    canMoveHere (row, col, b)
      = False, if b!row!col ~= Peg
      = or [checkDir (cO, rO) | (cO, rO) <- offsets], otherwise
        where

        || offsets denotes the different valid movement directions in the game.
        || I could have found a way to reuse the definitions at the top of the
        || program, but I didn't want to so I didn't :)

        offsets = [(0, -1), (1, 0), (0, 1), (-1, 0), (1, -1), (1, 1), (-1, 1), (-1, -1)]

        checkDir (cO, rO) = isType b row col Peg  1 (cO, rO) &
                            isType b row col Hole 2 (cO, rO)

    || isType :: pegboard -> num -> num -> pegstate -> num -> (num, num) -> bool

    || isType is used to make sure a particular cell is the expected valid type.
    || I made sure to do bounds checking first.

    isType b row col matchType scale (cO, rO)
      = newRow >= 0 & newRow < #b &
        newCol >= 0 & newCol < #(b!0) &
        b!newRow!newCol = matchType
        where
        newRow = row + scale * rO
        newCol = col + scale * cO

|| ======================================================================================
|| fn split
|| simple helper function. Can't for the life of me remember what it does since the
|| name is so ambiguous. Just kidding, it splits stuff.
|| ======================================================================================

split :: char -> [char] -> [[char]]

split splitter line
  = takewhile (~= splitter) line : rest
    where
    remainder = dropwhile (~= splitter) line
    rest = split splitter (drop 1 remainder), if #remainder > 0
         = [], otherwise

|| ======================================================================================
|| fn playMove
|| Takes a (board, score) tuple and movement, and returns an updated board and score.
|| Replaces the earlier 'play' function, which simulated a whole moveset.
|| ======================================================================================

playMove :: (pegboard, num) -> movement -> (pegboard, num)
playMove (board, score) move
  = xPlayMove board score move
    where
    boardsizeX = # board
    boardsizeY = # (board!0)
    xPlayMove b s m = ((makemove.checkmove) (b, m), s + 1)
    checkmove (b, (x , y, (xO, yO)))
      = error "moved off board",          if ((x + xO) < 1)
                                          \/ ((x + xO) > boardsizeX)
                                          \/ ((y + yO)) < 1
                                          \/ ((y + yO) > boardsizeY)
      = error "moved off board (invalid)",if ((b!(y + 2 * yO - 1))!(x + 2 * xO - 1) = Invalid)
      = error "no peg at given position", if ((b!(y - 1))!(x - 1) ~= Peg)
      = error "no peg to jump over",      if ((b!(y +     yO - 1))!(x +     xO - 1) ~= Peg)
      = error "no hole to jump into",     if ((b!(y + 2 * yO - 1))!(x + 2 * xO - 1) ~= Hole)
      = (b, (x, y, (xO, yO))),            otherwise
    makemove (b, (x, y, (xO, yO))) = ((make Peg (x + 2 * xO, y + 2 * yO)).(make Hole (x + xO, y + yO)).(make Hole (x, y))) b
    make p (x, y) b = (take (y - 1) b) ++ [(makerow p x (b!(y - 1)))] ++ (drop y b)
    makerow p x row = (take (x - 1) row) ++ [p] ++ (drop x row)

|| ======================================================================================
|| fn convertInputToMovement
|| Converts a user input to a movement by splitting the input, casting the first
|| two values into numbers and the third into a 'direction'
||
|| NB: There is no input validation here.
|| ======================================================================================

convertInputToMovement :: [char] -> movement
convertInputToMovement input
  = (x, y, setDir dir) 
    where
    splitInput = split ' ' input
    x = numval (splitInput!0)
    y = numval (splitInput!1)
    dir = splitInput!2
    setDir d
     = north,     if d = "north"     \/ d = "n"
     = east,      if d = "east"      \/ d = "e"
     = south,     if d = "south"     \/ d = "s"
     = west,      if d = "west"      \/ d = "w"
     = northeast, if d = "northeast" \/ d = "ne"
     = southeast, if d = "southeast" \/ d = "se"
     = southwest, if d = "southwest" \/ d = "sw"
     = northwest, if d = "northwest" \/ d = "nw"
     = error "Invalid move!", otherwise

|| ======================================================================================
|| fn runGame
|| The primary function to run the interactive game. Contains several helper functions
|| which share the input stream: getBoardName; doGameLoop
|| ======================================================================================

runGame
  = Stdout msg_init : getBoardName $-
    where msg_init = "Welcome to Soliteverett!\nPlease enter a file name to load a board: "

    || getBoardName :: [char] -> [sys_message]
    getBoardName input
      = Stdout msg_board : doGameLoop (board, 0) rest
        where
        boardName = filter (~= '\r') (takewhile (~= '\n') input)
        rest = drop 1 (dropwhile (~= '\n') input)

        board = defaultBoard, if #boardName = 0
              = loadBoard boardName, otherwise

        msg_board = "Selected board: " ++ boardName ++ ".\n", if #boardName > 0
                  = "No board selected. Using Default.\n", if #boardName = 0

    || doGameLoop :: (pegboard, num) -> [char] -> bool -> [sys_message]
    doGameLoop (currentBoard, score) input
      = [Stdout (printBoard (currentBoard, score) ++ "\nNo more valid moves!\nGame Over!\n")], if isGameOver
      = Stdout prompt : processInput input, otherwise
        where
        isGameOver = calculateGameOver currentBoard
        prompt = "\n" ++ (printBoard (currentBoard, score))
                      ++ "\nEnter your move (or 'q' to quit): "

        processInput stream
          = [Stdout "\nThanks for playing! Goodbye.\n"], if moveStr = "q"
          = doGameLoop nextState rest, otherwise
            where
            moveStr = filter (~= '\r') (takewhile (~= '\n') stream)
            rest = drop 1 (dropwhile (~= '\n') stream)

            parsedMove = convertInputToMovement moveStr
            nextState = playMove (currentBoard, score) parsedMove

|| ======================================================================================
|| fn main
|| I just like styling my code like this :)
|| ======================================================================================

main = runGame

|| ======================================================================================