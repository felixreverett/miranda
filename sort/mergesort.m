unsortedList
  = [5, 9, 1, 3, 7, 6, 2, 8, 4, 0,
     5, 9, 1, 3, 7, 6, 2, 8, 4, 0,
     5, 9, 1, 3, 7, 6, 2, 8, 4, 0,
     5, 9, 1, 3, 7, 6, 2, 8, 4, 0,
     5, 9, 1, 3, 7, 6, 2, 8, 4, 0,
     5, 9, 1, 3, 7, 6, 2, 8, 4, 0,
     5, 9, 1, 3, 7, 6, 2, 8, 4, 0,
     5, 9, 1, 3, 7, 6, 2, 8, 4, 0,
     5, 9, 1, 3, 7, 6, 2, 8, 4, 0,
     5, 9, 1, 3, 7, 6, 2, 8, 4, 0
    ]

main = show (mergesort unsortedList) ++ "\n"

|| ========================================================== ||
|| fn mergesort                                               ||
||                                                            ||
|| > Takes an unsorted list and divides it until only one     ||
||   element remains, then progressively sorts and merges     ||
||   each sublist using the sortSublists function.            ||
|| ========================================================== ||

mergesort :: [num] -> [num]

|| if the list has only one item, return that item

mergesort (any : []) = [any]

|| Else, sort the two halves of the list, making sure to
|| recursively mergesort each half first.

mergesort list
  = sortSublistsC (mergesort leftHalf) (mergesort rightHalf)
    where
    midpoint  = (#list) div 2
    leftHalf  = take midpoint list
    rightHalf = drop (midpoint) list

|| ========================================================== ||
|| fn sortSublists                                            ||
||                                                            ||
|| > Takes two sublists and combines them into a sorted list. ||
|| > Compares leftList[0] with rightList[0], and moves the    ||
||   smaller value to the sorted list. Repeats until one      ||
||   sublist is empty, then appends the other to the sorted   ||
||   list and returns it.                                     ||
|| ========================================================== ||

sortSublists :: [num] -> [num] -> [num]
sortSublists leftList rightList
  = xSortSublists leftList rightList [] || <-- sorted list
    where
    xSortSublists [] rL sList = sList ++ rL
    xSortSublists lL [] sList = sList ++ lL
    xSortSublists (left1 : lL) (right1 : rL) sList
      = xSortSublists lL (right1 : rL) (sList ++ [left1]),
        if left1 < right1
      = xSortSublists (left1 : lL) rL (sList ++ [right1]),
        otherwise

|| ========================================================== ||
|| fn sortSublistsB                                           ||
||                                                            ||
|| > Alternative accumulative implementation which appends to ||
||   a reverse-order list for significant efficiency gains.   ||
|| ========================================================== ||

sortSublistsB :: [num] -> [num] -> [num]
sortSublistsB leftList rightList
  = reverse (xSortSublists leftList rightList [])
    where
    xSortSublists [] rL sList = (reverse rL) ++ sList
    xSortSublists lL [] sList = (reverse lL) ++ sList
    xSortSublists (left1 : lL) (right1 : rL) sList
      = xSortSublists lL (right1 : rL) (left1 : sList),
        if left1 < right1
      = xSortSublists (left1 : lL) rL (right1 : sList),
        otherwise

|| ========================================================== ||
|| fn sortSublistsC                                           ||
||                                                            ||
|| > Accumulator-less version with max efficiency.            ||
|| ========================================================== ||

sortSublistsC :: [num] -> [num] -> [num]
sortSublistsC [] rL = rL
sortSublistsC lL [] = lL
sortSublistsC (left1 : lL) (right1 : rL)
  = left1 : sortSublistsC lL (right1 : rL), if left1 < right1
  = right1 : sortSublistsC (left1 : lL) rL, otherwise