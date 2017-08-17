module HanabiGuru.Client.Console.Presentation

open System
open HanabiGuru.Engine

let private task f x = fun () -> f x

let private presentPlayers = task (printfn "%A")

let private presentPlayerView =
    let presentNameTask (name : string) =
        let nameWidth = 10
        name |> Seq.truncate nameWidth |> String.Concat |> task (printf "%*s" nameWidth)
    let presentCardTask (Card (suit, Rank rank)) =
        [
            suit |> sprintf "%A" |> Seq.take 1 |> String.Concat |> task (printf "%s")
            rank |> task (printf "%i")
        ]
        |> List.reduce (>>)
    let weave x list =
        let insert item weavedList =
            if List.isEmpty weavedList
            then [item]
            else item :: x :: weavedList
        List.foldBack insert list []
    let presentHandTasks { player = Name name; cards = hand } =
        presentNameTask name
        :: task printf ": "
        :: weave (task printf " ") (List.map (presentCardTask) hand)
    let lineBreakTask = task printfn ""

    PlayerView.otherHands
    >> function
        | [] -> PlayerHand.create (PlayerIdentity.create "") [] |> List.singleton
        | hands -> hands
    >> List.map presentHandTasks
    >> List.map (List.reduce (>>))
    >> weave lineBreakTask
    >> List.reduce (>>)
    >> ((<<) lineBreakTask)

let game state =
    let present =
        match GameState.activePlayer state with
        | Some player -> GameState.playerView player state |> presentPlayerView
        | None -> GameState.players state |> presentPlayers
    present ()

let commandFailure (failure : CannotPerformAction) = printfn "%A" failure

let invalidInput = printfn "%s"
