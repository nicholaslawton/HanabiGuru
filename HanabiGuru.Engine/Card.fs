namespace HanabiGuru.Engine

type Suit =
    | Blue
    | Green
    | Red
    | White
    | Yellow

type Rank = Rank of int

type Card =
    | Card of Suit * Rank
