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
|| Returns True if no more valid moves are possible
|| Iterates over every row/col
||
|| ======================================================================================



calculateGameOver :: pegboard -> bool
calculateGameOver board
  = foldCol (0, 0, False) board
    where
    foldCol (r, c, any ) []             = any || converging case
    foldCol (r, c, any ) (front : rest) = foldCol (foldRow (r, c, any) front) rest

    foldRow (r, c, any ) []             = (r+1, 0, any) || converging case. Set col back to 0
    foldRow (r, c, any ) (front : rest) = foldRow (r, c+1, any & ~canMoveHere (r, c, board)) rest

    || canMoveHere :: (num, num, pegboard) -> bool
    canMoveHere (row, col, board)
      = False, if board!row!col ~= Peg
      = or [checkDir (cO, rO) | (cO, rO) <- offsets], otherwise
        where
        offsets = [(0, -1), (1, 0), (0, 1), (-1, 0), (1, -1), (1, 1), (-1, 1), (-1, -1)]
        checkDir (cO, rO)
          = isType board row col Peg  1 (cO, rO) &
            isType board row col Hole 2 (cO, rO)

    isType board row col matchType scale (cO, rO)
      = newRow >= 0 & newRow < #board &
        newCol >= 0 & newCol < #(board!0) &
        board!newRow!newCol = matchType
        where
        newRow = (row + scale * rO)
        newCol = (col + scale * cO)

|| ======================================================================================
|| fn split
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
      = [Stdout ("\nGame Over!\n")], if isGameOver
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
|| ======================================================================================

main = runGame

|| ======================================================================================