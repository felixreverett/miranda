|| ===============================================================================
|| A Game of Solitaire
|| This game simulates the 'peg solitaire' game, in which a player makes a set of
|| moves to remove as many pegs as possible by jumping one peg over another into
|| a valid hole.
|| 
|| The game uses an initial board 'initboard' and a pre-defined moveset 'moveset'
|| to simulate the progression of the game, displaying the game at its final
|| state.
|| ===============================================================================

|| The pegstate type represents the three possible states of each cell on a board.
|| Invalid signifies an "out-of-bounds" cell to allow for non-rectangular boards.

pegstate ::= Peg | Hole | Invalid



|| The direction type is used as an alias for the four valid move types

direction ::= N | S | E | W


|| board is a 2D array of 'pegstates', as defined above.

board == [[pegstate]]



|| initboard is the initial board state used to demonstrate the program

initboard :: board
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



|| movement consists of a tuple of x,y coordinates, plus a 'direction', where the
|| direction is defined as an algebraic type above.
|| I use a type synonym over an algebraic type because the data needs to be
|| deconstructed by the local function checkmove, rather than pattern matched

movement == (num, num, direction) || start x, start y, direction



|| moveset denotes test data. This will make three legal moves, the first of which
|| starts at 4, 6 (indexing from 1) and jumps North.

moveset
  = [
      (4, 6, N),
      (2, 5, E),
      (3, 7, N)
    ]



|| play is the main function of the program. It takes a list of movements and
|| folds them over a board, simulating the process of playing those moves.
|| 
|| Once this is done, it prints the board to the console using 'display'.

play :: [movement] -> [char]
play moves
  = display (foldl f initboard moves)
    where
    f b move = (makemove.checkmove) (b, move)

    || checkmove ensures moves are 'legal' by ensuring that the cells on the
    || board to update consist of a peg, a peg to jump over, and a hole to
    || jump to. The function also ensures the move does not go off-board.
    || If the move is valid, the program may continue, otherwise an error is
    || thrown.

    || checkmove :: movement -> movement

    checkmove (b,(x,y,m))
      = error "moved off board",          if ((y<3) & (m=N)) \/ ((y>5) & (m=S))
      = error "no peg at given position", if (b!(y-1))!(x-1) ~= Peg
      = error "no peg to jump over",      if ((m=N) & (b!(y-2))!(x-1) ~= Peg)
                                          \/ ((m=S) & (b!y)!(x-1) ~= Peg)
                                          \/ ((m=E) & (b!(y-1))!x ~= Peg)
                                          \/ ((m=W) & (b!(y-3))!(x-2) ~= Peg)
      = error ("no hole to jump into"),   if ((m=N) & (b!(y-3))!(x-1) ~= Hole)
                                          \/ ((m=S) & (b!(y+1))!(x-1) ~= Hole)
                                          \/ ((m=E) & (b!(y-1))!(x+1) ~= Hole)
                                          \/ ((m=W) & (b!(y-1))!(x-3) ~= Hole)
      = (b,(x,y,m)),                      otherwise

    || makemove processes a validated move and updates the board accordingly

    || makemove :: (board, movement) -> board

    makemove (b, (x,y,N))
      = ((make Peg (x, y-2)).(make Hole (x, y-1)).(make Hole (x, y))) b
    makemove (b, (x,y,S))
      = ((make Peg (x, y+2)).(make Hole (x, y+1)).(make Hole (x, y))) b
    makemove (b, (x,y,E))
      = ((make Peg (x+2, y)).(make Hole (x+1, y)).(make Hole (x, y))) b
    makemove (b, (x,y,W))
      = ((make Peg (x-2, y)).(make Hole (x+2, y)).(make Hole (x, y))) b

    || make changes the state of a board by updating a specific cell.
    || It leaves all rows except the target row unchanged, then calls
    || makerow to update the target row.

    || make :: pegstate -> (num, num) -> board -> board

    make p (x,y) b = (take (y-1) b) ++ [(makerow p x (b!(y-1)))] ++ (drop y b)

    || makerow :: pegstate -> num -> [pegstate] -> [pegstate]

    makerow p x row = (take (x-1) row) ++ [p] ++ (drop x row)

    || display takes the input board and maps each pegstate to a char

    || display :: board -> [char]

    display b 
      = lay (map ((map g)) b)
        where
        g Peg = 'O'
        g Invalid = ' '
        g any = '_'
|| ===============================================================================

main = play moveset