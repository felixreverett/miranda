|| =================================================================== ||
||   __  __ _                                                 _   _    ||
||  |  \/  (_)                                               | | | |   ||
||  | \  / |_ _ __   ___  _____      _______   _____ _ __ ___| |_| |_  ||
||  | |\/| | | '_ \ / _ \/ __\ \ /\ / / _ \ \ / / _ \ '__/ _ \ __| __| ||
||  | |  | | | | | |  __/\__ \\ V  V /  __/\ V /  __/ | |  __/ |_| |_  ||
||  |_|  |_|_|_| |_|\___||___/ \_/\_/ \___| \_/ \___|_|  \___|\__|\__| ||
||                                                                     ||
|| =================================================================== ||
|| Minesweverett, an implementation of Minesweeper
||
||
|| =============================================================

%include "../utils/utils_strings"
%include "../utils/io"

|| =============================================================
|| .data
||
|| cell is an AlgType used to 'mask' the celltype from the user.
||
|| cellType contains the individual values that a cell may be:
|| - Mine -> a mine
|| - Adjacent -> adjacent to N mines
|| - 
||
|| cellInit is used to simplify the creation of boards. The
|| actual cellTypes are evaluated when the baord is loaded
||
|| minefield is an alias for a 2D list of cells
|| =============================================================

cell ::= Hidden celltype | Shown celltype
celltype ::= Mine | Empty num

minefield == [[cell]]

defaultMinefield
  = (hideMinefield.flagMinefield.primeMinefield)
    ("E E E E E E E E E E\n" ++
     "E M E E E E E E M E\n" ++
     "E E E E E E E E E E\n" ++
     "E E E E E E E E E E\n" ++
     "E E E E E E E E E E\n" ++
     "E E E E E E E E E E\n" ++
     "E E E E E E E E E E\n" ++
     "E E E E E E E E E E\n" ++
     "E M E E E E E E M E\n" ++
     "E E E E E E E E E E\n")

|| =============================================================
|| fn printMinefield
|| =============================================================

printMinefield :: minefield -> [char]
printMinefield m
  = lay (map (map printCell) m)
    where
    printCell (Hidden any) = '?'
    printCell (Shown Mine) = '*'
    printCell (Shown (Empty 0)) = '_'
    printCell (Shown (Empty n)) = (show (n))!0

|| =============================================================
|| fn loadMinefield
||
|| > A series of functions that take a filename and convert it
||   into a 'primed' version of Mines and Empty,
||   then a 'flagged' version to mark the number of mines
||   adjacent to each Empty cell (if > 0),
||   and finally a 'hidden' version to make the board playable.
||
|| > I chose this step-by-step approach to facilitate debugging
||   and feature extension.
|| =============================================================

loadMinefield :: [char] -> minefield
loadMinefield filename
  = (hideMinefield
    .flagMinefield
    .primeMinefield
    .loadFile) filename



primeMinefield :: [char] -> [[celltype]]
primeMinefield fileContent
  = ( (map parseRow).(map (split " ")).lines) fileContent
    where
    parseRow row = map strToCell row
    strToCell "E" = Empty 0
    strToCell "M" = Mine



flagMinefield :: [[celltype]] -> [[celltype]]
flagMinefield primedfield
  = foldl flagCell primedfield coords
    where
    coords = [(r, c) | r <- [0 .. #primedfield - 1];
             c <- [0 .. #(primedfield!0) - 1]]

    flagCell field (row, col)
      = foldl flagAdjacents field coordSet,
          if isMine (row, col) field
      = field,
          otherwise
        where
        coordSet = [(row + a, col + b) | (a, b) <- offsets]
        offsets = [(0, -1), (1, 0), (0, 1), (-1, 0),
                  (1, -1), (1, 1), (-1, 1), (-1, -1)]
    
    flagAdjacents field (r, c)
      = makeFlag (r, c) field, if ((isInBounds (r, c) field)
                                  & (~isMine (r, c) field))
      = field, otherwise

    makeFlag (r, c) field
      = (take r field) ++
        [(makerow c (field!r) value)] ++
        (drop (r + 1) field)
        where
        value = (field!r)!c

    makerow c row (Empty amount)
      = (take c row)
        ++ [Empty (amount + 1)]
        ++ (drop (c + 1) row)

    isInBounds (r, c) field
      = True, if (r >= 0
                  & r < #field
                  & c >= 0
                  & c < #(field!r))
      = False, otherwise

    isMine (r, c) field
      = True, if (field!r)!c = Mine
      = False, otherwise



hideMinefield :: [[celltype]] -> minefield
hideMinefield constructed
  = map (map obfuscate) constructed
    where
    obfuscate any = Hidden any

|| =============================================================
|| fn run
|| =============================================================

run = (printMinefield.loadMinefield) "minefield1.txt"

|| =============================================================
|| fn main
|| =============================================================

main = run

|| =============================================================
