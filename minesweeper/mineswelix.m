|| ========================================================== ||
||       __  __ _                              _ _            ||
||      |  \/  (_)                            | (_)           ||
||      | \  / |_ _ __   ___  _____      _____| |___  __      ||
||      | |\/| | | '_ \ / _ \/ __\ \ /\ / / _ \ | \ \/ /      ||
||      | |  | | | | | |  __/\__ \\ V  V /  __/ | |>  <       ||
||      |_|  |_|_|_| |_|\___||___/ \_/\_/ \___|_|_/_/\_\      ||                                                                                    
||                                                            ||
|| ========================================================== ||
|| Mineswelix, a functional implementation of Minesweeper     ||
||                                                            ||
||                                                            ||
|| ========================================================== ||

%include "../utils/utils_strings"
%include "../utils/utils_lists"
%include "../utils/io"

|| =============================================================
|| Data
||
|| type cellType
|| > The individual values that a cell may be.
||   - Mine    -> a mine
||   - Empty n -> adjacent to n mines
||
|| type maskedCelltype
|| > Used to 'mask' the celltype from the user.
||
|| alias minefield
|| > a 2D list of maskedCelltype
||
|| alias coords
|| > A tuple of num used to index into the minefield.
||
|| alias cardinalOffsets
|| > NESW offset coords
||
|| alias offsets
|| > NESW + diagonal offset coords
|| =============================================================

celltype ::= Mine | Empty num

maskedCelltype ::= Hidden celltype | Shown celltype

minefield == [[maskedCelltype]]

defaultMinefield
  = (maskMinefield.flagMinefield.primeMinefield)
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
|| fn convertInput
||
|| > Converts an input into coords.
||
|| [!] No input validation
|| =============================================================

convertInput :: [char] -> coords

convertInput input
  = (coordify
    .(split " ")
    ) input
    where coordify n = (numval (n!0), numval (n!1))

|| =============================================================
|| fn playMove
||
|| > Takes a minefield and coords, and updates the board for
||   that move. Move conditions:
||   1) Mine -> game over
||   2) Empty (non zero) -> reveal that cell
||   3) Empty (zero) -> flood fill
||
|| > Assumes that the coords are already validated
|| =============================================================

playMove :: minefield -> coords -> minefield

playMove field (row, col)
  = playCell value
    where
    value = (field!row)!col
    playCell (Shown any)
      = error "Cannot play move on a shown cell"
    playCell (Hidden Mine)
      = error "Game over, you pressed a mine!"
    playCell (Hidden (Empty 0))
      = sweep field (row, col)
    playCell (Hidden any)
      = updateMinefield field (row, col) (Shown any)

|| =============================================================
|| fn sweep
||
|| > Sweeps across the board to reveal contiguous blocks of
||   Empty cells.
||
|| > Sweep on an "Empty 0" cell will recursively call adjacent
||   sweeps.
||
|| > Sweep on an "Empty n" cell will updateMinefield to reveal
||   that cell.
||
|| > Sweep on a "Mine" will return the current field, but
||   theoretically this shouldn't be possible because Mines are
||   always surrounded by "Empty n" cells.
|| =============================================================

sweep :: minefield -> coords -> minefield
sweep field (row, col)
  = field, if ~isInBounds (row, col) field
  = xSweep field (row, col) value, otherwise
    where
    value = (field!row)!col
    xSweep f (r, c) (Shown any) = f
    xSweep f (r, c) (Hidden Mine) = f
    xSweep f (r, c) (Hidden (Empty 0))
      = foldl sweep updatedField coordSet
        where
        updatedField
          = updateMinefield f (r, c) (Shown (Empty 0))
        coordSet = [(r + a, c + b) | (a, b) <- cardinalOffsets]
    xSweep f (r, c) (Hidden (Empty any))
      = updateMinefield f (r, c) (Shown (Empty any))

|| =============================================================
|| fn updateMinefield
|| > Updates a single cell on the minefield at the given coords.
|| =============================================================

updateMinefield :: minefield -> coords ->
                   maskedCelltype -> minefield

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
||   and finally a 'masked' version to make the board playable.
|| =============================================================

loadMinefield :: [char] -> minefield
loadMinefield filename
  = (maskMinefield
    .flagMinefield
    .primeMinefield
    .loadFile) filename

|| =============================================================
|| fn primeMinefield
||
|| > Converts the ascii representation of the minefield into a
||   field of Empty 0 or Mine celltypes.
|| =============================================================

primeMinefield :: [char] -> [[celltype]]
primeMinefield fileContent
  = ( (map parseRow).(map (split " ")).lines) fileContent
    where
    parseRow row = map strToCell row
    strToCell "E" = Empty 0
    strToCell "M" = Mine

|| =============================================================
|| fn flagMinefield
||
|| > Passes over every cell and flags every Empty cell adjacent
||   to a Mine. Flagging can occur more than once - a cell with
||   1 flag can be incremented to 2.
|| =============================================================

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

|| =============================================================
|| fn maskMinefield
||
|| > Masks all celltypes as Hidden.
|| =============================================================

maskMinefield :: [[celltype]] -> minefield
maskMinefield constructed
  = map (map obfuscate) constructed
    where
    obfuscate any = Hidden any

|| =============================================================
|| fn run
|| =============================================================

run
  = Stdout msg_init : getMinefieldName $-
    where msg_init
      = "Welcome to Mineswelix, a functional implementation"
        ++ " of minesweeper.\n\n"
        ++ "Please enter a file name to begin: "

getMinefieldName :: [char] -> [sys_message]
getMinefieldName input
  = Stdout msg_field : doGameLoop field rest
    where
    fieldName = filter (~= '\r') (takewhile (~= '\n') input)
    rest = drop 1 (dropwhile (~= '\n') input)

    field = defaultMinefield, if #fieldName = 0
          = loadMinefield fieldName, otherwise

    msg_field
      = "Selected minefield: "
        ++ fieldName
        ++ "\n", if #fieldName > 0
      = "No field selected. "
        ++ "Using default.\n", if #fieldName = 0

isGameOver field = False

doGameLoop :: minefield -> [char] -> [sys_message]
doGameLoop field input
  = [Stdout (printMinefield field)], if isGameOver field
  = Stdout prompt : processInput input, otherwise
    where
    prompt
      = "\n"
        ++ printMinefield field
        ++ "\nEnter your move, or press 'q' to quit: "

    processInput stream
      = [Stdout "\nThanks for playing! Goodbye.\n"], if moveStr = "q"
      = doGameLoop nextField rest, otherwise
        where
        moveStr = filter (~= '\r') (takewhile (~= '\n') stream)
        rest = drop 1 (dropwhile (~= '\n') stream)
        
        parsedMove = convertInput moveStr
        nextField = playMove field parsedMove

|| =============================================================
|| fn main
|| =============================================================

main = run

|| =============================================================