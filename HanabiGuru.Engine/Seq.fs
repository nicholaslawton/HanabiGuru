module Seq

let splitAt n xs = (Seq.truncate n xs, if n <= Seq.length xs then Seq.skip n xs else Seq.empty)
