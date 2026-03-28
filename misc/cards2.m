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

getSN :: card -> num
getSN (Spades,   any) = 3
getSN (Hearts,   any) = 2
getSN (Diamonds, any) = 1
getSN (Clubs,    any) = 0

|| We repeat this for numbers:

getNN :: card -> num
getNN (any, II)   = 0
getNN (any, III)  = 1
getNN (any, IV)   = 2
getNN (any, V)    = 3
getNN (any, VI)   = 4
getNN (any, VII)  = 5
getNN (any, VIII) = 6
getNN (any, IX)   = 7
getNN (any, X)    = 8
getNN (any, J)    = 9
getNN (any, Q)    = 10
getNN (any, K)    = 11
getNN (any, A)    = 12

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
                       suitnum = getSN c
                       numbernum = getNN c

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

|| compareHands takes two lists of hands and compares them to determine which hand
|| has the higher score. Each hand is is given a hand score of the form
|| ( hand_type_score, highest_card_score, suit_score )
|| where hand_type_score ranges from 4 "straight flush" to 0 "junk", and
|| where highest_card_score is 0-12 for straight flush and 4OAKs, but is
|| triplet_val * 13 + pair_val for full house to encode both scores in the simulate
|| object size. This is important because it allows us to use the second
|| value in the triple for multiple encodings, where reading the first value
|| means we never read the value wrong. ... AND where
|| highest_card_score is highest_card * 13 **4 + second * 13**3 + third *13**2 +
|| fourth * 13 + fifth.
|| Lastly, we encode the suit score from 0-3.
|| We also need to identify a hand's properties. The first step is to sort them
|| in descending order.
|| We use this to check for a 4OAK
|| 4OAK if A=B=C=D E or A B=C=D=E (when sorted)
|| if 4OAK, we don't care about the suit score, and we can already return the
|| other values of the triple.

|| We then check for FH:
|| FH is A=B=C D=E or A=B C=D=E (when sorted)
|| Once again, we don't care about suit score, but we must identify the
|| triplet. We do this via a logic check.

|| We then check for flushes and straight flushes together. We can use a bool
|| to track straight and current suit. Then, since we know the list is sorted,
|| we simply need to check that the values descend.

|| EZ. Now let's code it :)

|| compareHands takes two hands and calculates their hand scores with getHandScore
|| The function then uses these scores to determine the stronger hand.

hand == [card]

compareHands :: hand -> hand -> hand
compareHands hand1 hand2
  = hand1, if (handScore h1)        > (handScore h2)
  = hand2, if (handScore h1)        < (handScore h2)
  = hand1, if (highestCardScore h1) > (highestCardScore h2)
  = hand2, if (highestCardScore h1) < (highestCardScore h2)
  = hand1, if (suitScore h1)        > (suitScore h2)
  = hand2, if (suitScore h1)        < (suitScore h2)
  = error "No winning hand.", otherwise
    where
    handScore        (a, b, c) = a
    highestCardScore (a, b, c) = b
    suitScore        (a, b, c) = c
    h1 = getHandScore hand1
    h2 = getHandScore hand2

|| getHandScore takes a hand and returns a hand score.

handscore == (num, num, num)

getHandScore :: hand -> handscore

getHandScore h
  = xgetHandScore (sortHandDesc h)
    where
    xgetHandScore sh
      = (3, getNN (sh!2), 0) || get the highest card score from the middle card because this will always be part of the 4OAK
        , if getNN (sh!0) = getNN (sh!1) &
             getNN (sh!1) = getNN (sh!2) &
             getNN (sh!2) = getNN (sh!3) \/
             getNN (sh!1) = getNN (sh!2) &
             getNN (sh!2) = getNN (sh!3) &
             getNN (sh!3) = getNN (sh!4)
      = (2, getNN (sh!0) * 13 + getNN (sh!4), 0)
        , if getNN (sh!0) = getNN (sh!1) &
             getNN (sh!1) = getNN (sh!2) &
             getNN (sh!3) = getNN (sh!4)
      = (2, getNN (sh!4) * 13 + getNN (sh!0), 0)
        , if getNN (sh!0) = getNN (sh!1) &
             getNN (sh!2) = getNN (sh!3) &
             getNN (sh!3) = getNN (sh!4)
      = flushOrStraightFlush sh
        , if getSN (sh!0) = getSN (sh!1) &
             getSN (sh!1) = getSN (sh!2) &
             getSN (sh!2) = getSN (sh!3) &
             getSN (sh!3) = getSN (sh!4)
      = (0, 0, 0), otherwise
    flushOrStraightFlush sh
      = (4, polyScore, getSN (sh!0))
        , if getNN (sh!0) = 1 + getNN (sh!1) &
             getNN (sh!1) = 1 + getNN (sh!2) &
             getNN (sh!2) = 1 + getNN (sh!3) &
             getNN (sh!3) = 1 + getNN (sh!4)
      = (1, polyScore, getSN (sh!0))
        , otherwise
        where
        polyScore
          = getNN (sh!0) * (13 * 13 * 13 * 13) +
            getNN (sh!1) * (13 * 13 * 13) +
            getNN (sh!2) * (13 * 13) +
            getNN (sh!3) * 13 +
            getNN (sh!4)

|| sortHandDesc

sortHandDesc :: hand -> hand
sortHandDesc h
  = xSHD h
    where
    xSHD [] = []
    xSHD (x : xs)
      = xSHD larger ++ [x] ++ xSHD smaller
        where
        larger  = [x2 | x2 <- xs; getNN x2 >= getNN x]
        smaller = [x2 | x2 <- xs; getNN x2 <  getNN x]



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

|| main = deal 4 5 (shuffle 1 example_deck myfunc)
