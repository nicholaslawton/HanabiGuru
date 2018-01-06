module List

let splitBy predicate =
    let rec splitByStep splits xs =
        match splits, xs with
        | [], _ -> splitByStep ([] :: splits) xs
        | _, [] -> splits |> List.rev |> List.map List.rev
        | _, pivot :: tail when predicate pivot -> splitByStep ([] :: splits) tail
        | currentSplit :: previousSplits, head :: tail -> splitByStep ((head :: currentSplit) :: previousSplits) tail
    splitByStep []

let weave x list =
    let insert item weavedList =
        if List.isEmpty weavedList
        then [item]
        else item :: x :: weavedList
    List.foldBack insert list []

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

let randomItem randomInt xs =
    if xs = [] then failwith "empty list"
    List.item (randomInt 0 (List.length xs)) xs
