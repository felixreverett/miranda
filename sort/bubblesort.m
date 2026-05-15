|| =============================================================
|| Bubble Sort
||
|| Takes an unsorted list and primes an auxiliary function with
|| two initial pointer values. The leftPointer inits to 0, and
|| the rightPointer inits to the final index of the list.
||
|| The function walks the leftPointer through the list,
|| comparing the two values lv and lr at index leftPointer and
|| index leftPointer + 1, and swapping them if lr < lv. Once
|| it reaches the rightPointer, it decreases the rightPointer
|| by 1. This repeats until the rightPointer equals zero, at
|| which point the list is sorted and we can call the
|| converging case.
|| =============================================================

unsortedList = [5, 9, 1, 3, 7, 6, 2, 8, 4, 0]

bubblesort :: [num] -> [num]
bubblesort list

    || xBubblesort :: [num] -> num -> num -> [num]
    || converges if rightPointer is 0
    || reduces rightPointer if leftPointer is adjacent
    || else calls xBubblesort + updateList on the list

  = xBubblesort list 0 ((#list)-1)
    where
    xBubblesort li leftPointer 0  = li
    xBubblesort li leftPointer rightPointer
      = xBubblesort li 0 (rightPointer-1), if (leftPointer + 1) >= rightPointer
      = xBubblesort (updateList li leftPointer) (leftPointer + 1) rightPointer, otherwise
    
    || updateList :: [num] -> num -> [num]
    
    updateList l lp
      = xUpdateList l leftVal rightVal
        where

        leftVal = l!lp
        rightVal = l!(lp+1)

        xUpdateList l lv rv
          = take lp l ++ [rv] ++ [lv] ++ drop (lp+2) l, if lv > rv
          = l, otherwise

main = show (bubblesort unsortedList) ++ "\n"