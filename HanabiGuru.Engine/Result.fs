module Result

let collect listOfResults =
    let collectStep (xs, errors) x =
        match errors, x with
        | [], Ok x -> (x :: xs, errors)
        | _, Error error -> ([], error :: errors)
        | _, Ok _ -> ([], errors)
    match List.fold collectStep ([], []) listOfResults with
    | xs, [] -> xs |> List.rev |> Ok
    | _, errors -> errors |> List.rev |> Error
