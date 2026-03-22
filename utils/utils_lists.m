|| ==========================================================================================================
|| utils.lists
|| ==========================================================================================================

vector2D == (num, num)

|| ==========================================================================================================
|| fn isInBounds
||
|| > Generic function which determines whether a set of coords
||   are within the bounds of a 2D list
|| ==========================================================================================================

isInBounds :: vector2D -> [[*]] -> bool
isInBounds (r, c) array
  = True,  if (r >= 0 & r < #array & c >= 0 & c < #(array!r))
  = False, otherwise

|| ==========================================================================================================
|| fn buildIndexers
||
|| > Build a list of indexers using list comprehensions, specifically for a 2D array
||
|| > NB: supports jagged arrays
|| ==========================================================================================================

buildIndexers :: [[*]] -> [vector2D]
buildIndexers array
  = [(a, b) | a <- [0..#array-1]; b <- [0..#(array!a)-1]]