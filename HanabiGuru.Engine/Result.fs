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

let combine f combineErrors xOrError yOrError =
    match xOrError, yOrError with
    | Ok x, Ok y -> f x y |> Ok
    | Error error, Ok _ -> Error error
    | Ok _, Error error -> Error error
    | Error xError, Error yError -> combineErrors (xError, yError) |> Error
