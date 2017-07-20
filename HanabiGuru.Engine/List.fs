module List

let extract target xs = 
    let extractionStep (extraction, remainder) x =
        match extraction with
        | Some _ -> (extraction, x :: remainder)
        | None when x = target -> (Some x, remainder)
        | None -> (None, x :: remainder)
    List.fold extractionStep (None, []) xs
    |> Pair.mapSnd List.rev

let remove x = extract x >> snd

let removeEach xsToRemove xs =
    let addOrRemove (xs, xsToRemove) x =
        if List.contains x xsToRemove
        then (xs, remove x xsToRemove)
        else (x :: xs, xsToRemove)
    List.fold addOrRemove ([], xsToRemove) xs |> fst |> List.rev
