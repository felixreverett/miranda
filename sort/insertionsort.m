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

main = show (iSort unsortedList) ++ "\n"

|| ========================================================== ||
|| fn insertionsort                                           ||
||                                                            ||
|| ========================================================== ||

iSort :: [num] -> [num]

iSort list
  = xISort list [] || newlist here
    where

    || Base case. If the list-to-sort is empty, return newList
    xISort [] newList = reverse newList

    || Recursive case. extract the head of the list-to-sort
    || and call placeInList, which puts it in reverse order
    || in the newList. We fix this later with reverse

    xISort (x : xs) newList
      = xISort xs (placeInList newList x)
    
    || If the newList is empty, the newVal is the newList
    placeInList [] newVal = newVal : []

    || Compare newVal to head of list, placing it in front
    || if the newVal is larger, otherwise 'swapping' them.

    placeInList (x : xs) newVal
      = newVal : (x:xs), if (newVal >= x)
      = x : (placeInList xs newVal), otherwise