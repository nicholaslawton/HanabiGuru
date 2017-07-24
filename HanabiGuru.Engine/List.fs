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

let update predicate f xs = List.foldBack (fun x xs -> (if predicate x then f x else x) :: xs) xs []

let randomItem randomInt xs = List.item (randomInt 0 (List.length xs)) xs
