|| ========================================================== ||
||   __  __ _                              _ _                ||
||  |  \/  (_)                            | (_)               ||
||  | \  / |_ _ __   ___  _____      _____| |___  __          ||
||  | |\/| | | '_ \ / _ \/ __\ \ /\ / / _ \ | \ \/ /          ||
||  | |  | | | | | |  __/\__ \\ V  V /  __/ | |>  <           ||
||  |_|  |_|_|_| |_|\___||___/ \_/\_/ \___|_|_/_/\_\          ||                                                                                    
||                                                            ||
|| ========================================================== ||
|| Mineswelix, a functional implementation of Minesweeper     ||
||                                                            ||
||                                                            ||
|| ========================================================== ||

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

coords == (num, num)

cardinalOffsets
  = [(0, -1), (1, 0), (0, 1), (-1, 0)]

offsets
  = cardinalOffsets ++ [(1, -1), (1, 1), (-1, 1), (-1, -1)]

|| =============================================================
|| fn playMove
|| > Takes a minefield and coords, and updates the board for
||   that move.
|| > Move conditions:
||   1) Mine -> game over
||   2) Empty (non zero) ->
||   3) Empty (zero) -> flood fill
||
|| > Assumes that the coords are valid
|| =============================================================

playMove :: minefield -> coords -> minefield

playMove field (row, col)
  = playCell value
    where
    value = (field!row)!col
    playCell (Shown any) = error "Cannot play move on a shown cell"
    playCell (Hidden Mine) = error "Game over, you pressed a mine!"
    playCell (Hidden (Empty 0)) = sweep field (row, col)
    playCell (Hidden any) = updateMinefield field (row, col) (Shown any)

|| =============================================================
|| fn sweep
|| > Sweeps across the board to reveal contiguous blocks of
||   Empty cells.
||
|| > Sweep on an "Empty 0" cell will recursively call adjacent
||   sweeps.
|| > Sweep on an "Empty n" cell will updateMinefield to reveal
||   that cell.
|| > Sweep on a "Mine" will return the current field, but
||   theoretically this shouldn't be possible because Mines are
||   always surrounded by "Empty n" cells.
||
|| =============================================================

sweep :: minefield -> coords -> minefield
sweep field (row, col)
  = field, if ~isInBounds (row, col) field
  = xSweep field (row, col) value, otherwise
    where
    value = (field!row)!col
    xSweep f (r, c) (Shown any) = f
    xSweep f (r, c) (Hidden Mine) = error "how did we get here" || f
    xSweep f (r, c) (Hidden (Empty 0))
      = foldl sweep updatedField coordSet
        where
        updatedField = updateMinefield f (r, c) (Shown (Empty 0))
        coordSet = [(r + a, c + b) | (a, b) <- cardinalOffsets]
    xSweep f (r, c) (Hidden (Empty any))
      = updateMinefield f (r, c) (Shown (Empty any))

|| =============================================================
|| fn updateMinefield
|| > Updates a single cell on the minefield at the given coords.
|| =============================================================

updateMinefield :: minefield -> coords -> cell -> minefield

updateMinefield field (row, col) value
  = (take row field) ++
    [(makerow col (field!row) value)] ++
    (drop (row + 1) field)
    where
    makerow col r value
      = (take col r) ++
        [value] ++
        (drop (col + 1) r)

|| =============================================================
|| fn printMinefield
|| > takes a minefield and converts it into a string suitable
||   for printing to the console.
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

    isMine (r, c) field
      = True, if (field!r)!c = Mine
      = False, otherwise



hideMinefield :: [[celltype]] -> minefield
hideMinefield constructed
  = map (map obfuscate) constructed
    where
    obfuscate any = Hidden any

|| =============================================================
|| fn isInBounds
|| > Generic function which determines whether a set of coords
||   are within the bounds of a 2D list
|| =============================================================

isInBounds :: coords -> [[*]] -> bool
isInBounds (r, c) array
  = True,  if (r >= 0 & r < #array & c >= 0 & c < #(array!r))
  = False, otherwise

|| =============================================================
|| fn run
|| =============================================================

run = printMinefield (playMove (loadMinefield "minefield1.txt") (5, 5))

|| =============================================================
|| fn main
|| =============================================================

main = run

|| =============================================================
