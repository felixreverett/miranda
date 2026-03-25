suit ::= Spades | Hearts | Diamonds | Clubs

number ::= Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | J | Q | K | A

card == (suit, number)

example_deck
  = [
    (Spades, Two), (Spades, Three), (Spades, Four), (Spades, Five), (Spades, Six), (Spades, Seven), (Spades, Eight), (Spades, Nine), (Spades, Ten), (Spades, J), (Spades, Q), (Spades, K), (Spades, A),
    (Hearts, Two), (Hearts, Three), (Hearts, Four), (Hearts, Five), (Hearts, Six), (Hearts, Seven), (Hearts, Eight), (Hearts, Nine), (Hearts, Ten), (Hearts, J), (Hearts, Q), (Hearts, K), (Hearts, A),
    (Diamonds, Two), (Diamonds, Three), (Diamonds, Four), (Diamonds, Five), (Diamonds, Six), (Diamonds, Seven), (Diamonds, Eight), (Diamonds, Nine), (Diamonds, Ten), (Diamonds, J), (Diamonds, Q), (Diamonds, K), (Diamonds, A),
    (Clubs, Two), (Clubs, Three), (Clubs, Four), (Clubs, Five), (Clubs, Six), (Clubs, Seven), (Clubs, Eight), (Clubs, Nine), (Clubs, Ten), (Clubs, J), (Clubs, Q), (Clubs, K), (Clubs, A)
    ]

shuffle n deck
  = xshuffle n deck
    where
    xshuffle 0 d = d
    xshuffle any d = xshuffle (any - 1) (interleave (cut d))

    || cut :: [card] -> ([card], [card])
    cut d
      = (haskell, curry)
        where
        haskell = take half d
        curry   = drop half d
        half    = lengf div 2, if lengf mod 2 = 0
                = (lengf+1) div 2, otherwise
        lengf   = #d

    interleave (a, b)
      = xinterleave (a, b) []
        where
        xinterleave ([], [])           any = any
        xinterleave ([], y )           any = any ++ y
        xinterleave (x , [])           any = any ++ x
        xinterleave ((x1:xs), (y1:ys)) any = xinterleave (xs, ys) (any ++ [y1] ++ [x1])

main = (shuffle 10 example_deck)!0
