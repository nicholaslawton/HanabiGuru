namespace HanabiGuru.Client.Console

open HanabiGuru.Engine

type Command =
    | AddPlayer of string
    | StartGame
    | GiveInformation of string * CardTrait
    | DiscardCard of char
    | PlayCard of char

type InvalidCommandReason =
    | InvalidCardTag

type CommandError =
    | InvalidCommand of InvalidCommandReason list
    | ExecutionFailure of CannotPerformAction

module Command =

    let cardTag i = (int)'a' + i |> char
    let cardIndex (c : char) = (int)c - (int)'a'

    let selectCard tag cards =
        List.indexed cards
        |> List.map (Pair.mapFst cardTag)
        |> List.filter (fst >> (=) tag)
        |> List.map snd
        |> List.tryHead
        |> Option.map Ok
        |> Option.defaultValue (Error (InvalidCommand [InvalidCardTag]))

    let execute game =
        let cardPlayAction cardId action game =
            let getActivePlayerHand game =
                GameState.activePlayer game
                |> Option.map (fun activePlayer -> GameState.playerView activePlayer game |> PlayerView.hand |> Ok)
                |> Option.defaultValue (Error (ExecutionFailure (CannotTakeTurn [GameNotStarted])))

            getActivePlayerHand game
            |> Result.bind (selectCard cardId)
            |> Result.bind (fun card -> action card game |> Result.mapError ExecutionFailure)

        function
        | AddPlayer name ->
            Game.addPlayer (PlayerIdentity.create name) game |> Result.mapError ExecutionFailure
        | StartGame ->
            Game.startGame game |> Result.mapError ExecutionFailure
        | GiveInformation (name, cardTrait) ->
            Game.giveInformation (PlayerIdentity.create name) cardTrait game |> Result.mapError ExecutionFailure
        | DiscardCard cardId -> cardPlayAction cardId Game.discard game
        | PlayCard cardId -> cardPlayAction cardId Game.playCard game
