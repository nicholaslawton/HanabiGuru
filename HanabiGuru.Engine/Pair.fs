module Pair

let mapFst f (fst, snd) = (f fst, snd)

let mapSnd f (fst, snd) = (fst, f snd)
