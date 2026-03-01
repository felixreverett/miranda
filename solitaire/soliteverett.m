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



|| ==========================================================================================================
|| This is the Felix implementation of Solitaire, also known as 'Soliteverett'
||
|| Major Changes:
|| - Optimised logic in checkmove and makemove to use directional tuples instead;
|| - Added dynamic boardsizing. This requires that the array not be jagged;
|| - You can play diagonally;
|| - Added a score to the accumulator. Yes you could just type `# moves`,
||   but the score is incremented in the accumulator.
|| - Added file loading and parsing of text into a pegboard.
||
|| Fixes:
|| - The initial implementation did not prevent off-board moves East or West;
|| - Added explicit error checking to ensure moves cannot land on an invalid tile,
||   which was previously handled incidentally by ensuring the destination was a Hole.
||
|| ==========================================================================================================



pegstate ::= Peg | Hole | Invalid

|| Values to denote movement offsets (to simplify logic in play)

north     = ( 0, -1)
east      = ( 1,  0)
south     = ( 0,  1)
west      = (-1,  0)
northeast = ( 1, -1)
southeast = ( 1,  1)
southwest = (-1,  1)
northwest = (-1, -1)

|| Initial board state. This can be modified.

initboard
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

|| the moveset for the game

moves =
  [
    (4, 6, north),
    (2, 5, east),
    (3, 7, north),
    (5, 4, southwest),
    (4, 7, northwest)
  ]

|| ==========================================================================================================
|| fn play
|| ==========================================================================================================

play :: [movement] -> [char]
play moves
	= printBoard (foldl f (initboard, 0) moves)
		where
          || I had to indent it like this because it wouldn't compile otherwise?
          boardsizeX = # initboard
          boardsizeY = # (initboard!0)
		f (b, score) move = ((makemove.checkmove) (b, move), score + 1)
		checkmove (b, (x , y, (xO, yO)))
          = error "moved off board",          if ((x + xO) < 1) \/ ((x + xO) > boardsizeX) \/ ((y + yO)) < 1 \/ ((y + yO) > boardsizeY)
          = error "moved off board (invalid)",if ((b!(y + 2 * yO - 1))!(x + 2 * xO - 1) = Invalid)
          = error "no peg at given position", if ((b!(y - 1))!(x - 1) ~= Peg)
          = error "no peg to jump over",      if ((b!(y +     yO - 1))!(x +     xO - 1) ~= Peg)
          = error "no hole to jump into",     if ((b!(y + 2 * yO - 1))!(x + 2 * xO - 1) ~= Hole)
          = (b, (x, y, (xO, yO))),            otherwise
    makemove (b, (x, y, (xO, yO))) = ((make Peg (x + 2 * xO, y + 2 * yO)).(make Hole (x + xO, y + yO)).(make Hole (x, y))) b
    make p (x, y) b = (take (y - 1) b) ++ [(makerow p x (b!(y - 1)))] ++ (drop y b)
    makerow p x row = (take (x - 1) row) ++ [p] ++ (drop x row)

|| ==========================================================================================================
|| fn printBoard
|| ==========================================================================================================

printBoard (b, score) 
      = (lay (map (concat.(map g)) b)) ++ "\nScore: " ++ show (score + 0)
        where
        g Peg = "O"
        g Invalid = " "
        g any = "_"

|| ==========================================================================================================
|| fn loadBoard
|| ==========================================================================================================

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

|| ==========================================================================================================
|| fn split
|| ==========================================================================================================

split :: char -> [char] -> [[char]]

split splitter line
  = takewhile (~= splitter) line : rest
    where
    remainder = dropwhile (~= splitter) line
    rest = split splitter (drop 1 remainder), if #remainder > 0
         = [], otherwise

|| ==========================================================================================================
|| fn main
|| ==========================================================================================================

main :: [char]
|| main = play moves

|| main = printBoard ((loadBoard "pegboard.txt"),0)
main = printBoard ((loadBoard "diamondboard.txt"),0)


|| ==========================================================================================================
