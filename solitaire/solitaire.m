|| A Game of Solitaire

pegstate ::= Peg | Hole | Invalid

direction ::= N | S | E | W

board == [[pegstate]]

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

movement == (num, num, direction) || start x, start y, direction

moves
  = [
      (4, 6, N),
      (2, 5, E),
      (3, 7, N)
    ]

|| =========================================================================================
play :: [movement] -> [char]
play moves
	= display (foldl f initboard moves)
		where
		f b move = (makemove.checkmove) (b, move)
    || For some reason I can't get indentation any less than the below to compile for 'checkmove'
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
    makemove (b, (x,y,N)) = ((make Peg (x, y-2)).(make Hole (x, y-1)).(make Hole (x, y))) b
    makemove (b, (x,y,S)) = ((make Peg (x, y+2)).(make Hole (x, y+1)).(make Hole (x, y))) b
    makemove (b, (x,y,E)) = ((make Peg (x+2, y)).(make Hole (x+1, y)).(make Hole (x, y))) b
    makemove (b, (x,y,W)) = ((make Peg (x-2, y)).(make Hole (x+2, y)).(make Hole (x, y))) b
    make p (x,y) b = (take (y-1) b) ++ [(makerow p x (b!(y-1)))] ++ (drop y b)
    makerow p x row = (take (x-1) row) ++ [p] ++ (drop x row)
    display b 
      = lay (map (concat.(map g)) b)
        where
        g Peg = "O"
        g Invalid = " "
        g any = "_"
|| =========================================================================================

main = play moves