module Pair

let map f (fst, snd) = (f fst, f snd)

let mapFst f (fst, snd) = (f fst, snd)

let mapSnd f (fst, snd) = (fst, f snd)
