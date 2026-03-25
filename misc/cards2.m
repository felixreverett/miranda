|| A reprsentation of the four possible suits
suit ::= Spades | Hearts | Diamonds | Clubs

|| The thirteen possible card types/values. These are not explicitly
|| given corresponding num values
number ::= II | III | IV | V | VI | VII | VIII | IX | X | J | Q | K | A

|| A type synonym to denote a card
card == (suit, number)

|| The shuffle function takes a number (the times to make a shuffle),
|| a list of cards, and a function that will compare any two cards.

|| First, the function ensures the number is > 0 to prevent it looping forever and
|| to guarantee it runs at least once.
|| Next, the function validates the input to ensure <= 52 cards and that every
|| card is unique. Finally, the function shuffles the deck n times.

shuffle :: num -> [card] -> (card -> card -> bool) -> [card]

shuffle n deck fn
  = error "Cannot shuffle deck fewer than 1 times", if n < 1
  = error "Deck must contain at most 52 cards", if #deck > 52
  = error "Deck cannot be empty", if #deck < 1
  = error "All cards must be unique", if ~isDeckUnique deck
  = xshuffle n deck, otherwise
    where
    || Base case. Note that we could allow 0 shuffles, but opt to enforce >= 1.
    xshuffle 0 d = d
    || An added benefit of using xshuffle is accumulate recursion, which can be
    || tail-call optimised to keep the stack nice and happy.
    xshuffle any d = xshuffle (any-1) ((interleave.cut) d )

    || cut splits a deck in two halves and returns a two-tuple. if an
    || unequally-sized half exists this will be the first half. This is
    || done by checking if the length is even.
    || cut :: [card] -> ([card], [card])
    cut d = (front, back)
            where
            front = take half d
            back = drop half d
            half = len div 2, if len mod 2 = 0
                 = (len+1) div 2, otherwise
            len = #d

    || interleave interleaves two decks into one, starting with the first element
    || in the second deck

    || interleave :: ([card], [card]) -> [card]
    interleave (a, b)
      = xinterleave (a, b) []
        where
        xinterleave ([], []) any = any || base case
        xinterleave ([], y) any = any ++ y || technically not possible
        xinterleave (x, []) any = any ++ x
        xinterleave ((x:xs), (y:ys)) any
          = xinterleave (xs, ys) (any ++ [y] ++ [x])

|| isDeckUnique uses the properties of a deck of cards to efficiently determine
|| whether any duplicate cards are present. It need only make at most 52
|| checks and 52 updates.
|| First, we map suits and numbers to num values (integers):

getSuitNum :: card -> num
getSuitNum (Spades,   any) = 3
getSuitNum (Hearts,   any) = 2
getSuitNum (Diamonds, any) = 1
getSuitNum (Clubs,    any) = 0

|| We repeat this for numbers:

getNumberNum :: card -> num
getNumberNum (any, II)   = 0
getNumberNum (any, III)  = 1
getNumberNum (any, IV)   = 2
getNumberNum (any, V)    = 3
getNumberNum (any, VI)   = 4
getNumberNum (any, VII)  = 5
getNumberNum (any, VIII) = 6
getNumberNum (any, IX)   = 7
getNumberNum (any, X)    = 8
getNumberNum (any, J)    = 9
getNumberNum (any, Q)    = 10
getNumberNum (any, K)    = 11
getNumberNum (any, A)    = 12

|| Now for the fun part! We will simulate a bitmap of length 52, initialised to
|| false, and we will use the above maps to see if a value has already
|| been encountered.

isDeckUnique deck
  = xIDU deck bitmap
    where
    bitmap = [False | x <- [0..51]]
    xIDU [] any = True
    xIDU ( c: cs) bm = False, if bm!index
                     = xIDU cs (updateBitmap bm index), otherwise
                       where
                       index = suitnum * 13 + numbernum
                       suitnum = getSuitNum c
                       numbernum = getNumberNum c

|| updateBitmap is used to produce a new bitmap

updateBitmap :: [bool] -> num -> [bool]
updateBitmap bm n
  = (take n bm) ++ [True] ++ (drop (n+1) bm)

|| deal takes 4 sets of 5 cards from the deck

myDeal = deal 4 5 example_deck

deal :: num -> num -> [card] -> [[card]]

deal numHands handSize deck
  = xdeal numHands handSize deck
    where
    xdeal nH hS d
      = error ("Cannot draw " ++ show(nH) ++ " hands of "
               ++ show(hS) ++ " cards from deck."), if (nH*hS > #d) \/ nH < 1 \/ hS < 1
      = ydeal nH hS d [], otherwise
    
    ydeal 0  hS d hands = hands
    ydeal nH hS d hands = ydeal (nH-1) hS (drop hS d) ((take hS d) : hands)

|| =============================================

example_deck
  = [
    (Spades, II), (Spades, III), (Spades, IV), (Spades, V), (Spades, VI), (Spades, VII), (Spades, VIII), (Spades, IX), (Spades, X), (Spades, J), (Spades, Q), (Spades, K), (Spades, A),
    (Hearts, II), (Hearts, III), (Hearts, IV), (Hearts, V), (Hearts, VI), (Hearts, VII), (Hearts, VIII), (Hearts, IX), (Hearts, X), (Hearts, J), (Hearts, Q), (Hearts, K), (Hearts, A),
    (Diamonds, II), (Diamonds, III), (Diamonds, IV), (Diamonds, V), (Diamonds, VI), (Diamonds, VII), (Diamonds, VIII), (Diamonds, IX), (Diamonds, X), (Diamonds, J), (Diamonds, Q), (Diamonds, K), (Diamonds, A),
    (Clubs, II), (Clubs, III), (Clubs, IV), (Clubs, V), (Clubs, VI), (Clubs, VII), (Clubs, VIII), (Clubs, IX), (Clubs, X), (Clubs, J), (Clubs, Q), (Clubs, K), (Clubs, A)
    ]

myfunc :: card -> card -> bool
myfunc (a, b) (c, d) = (a = c) & (b = d)

main = deal 4 5 (shuffle 1 example_deck myfunc)