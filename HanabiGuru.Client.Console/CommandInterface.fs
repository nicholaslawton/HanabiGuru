module HanabiGuru.Client.Console.CommandInterface

open FParsec
open System
open HanabiGuru.Engine

let processInput getInput pipeline =
    let events = new Event<_> ()
    let rec getNextInput () =
        printf "> "
        getInput ()
        |> Option.iter (fun input ->
            events.Trigger input
            getNextInput () |> ignore)
    let subscriptions = events.Publish |> pipeline
    getNextInput ()
    subscriptions |> List.iter (fun (subscription : IDisposable) -> subscription.Dispose())

let parse input =
    let name = many1Chars anyChar <?> "player name"
    let addPlayer =
        let prefix = skipString "add" >>. spaces >>. skipString "player" <?> "add player"
        prefix >>. spaces >>. name |>> AddPlayer
    let startGame = stringReturn "start" StartGame
    let giveInformation =
        let suitTrait = 
            [Blue; Green; Red; White; Yellow]
            |> List.map (fun suit -> stringReturn (sprintf "%A" suit) (SuitTrait suit))
            |> choice
            <?> "colour"
        let rankTrait = pint32 |>> (Rank >> RankTrait) <?> "number"
        let cardTrait = suitTrait <|> rankTrait
        skipString "tell" >>. spaces >>. cardTrait .>> spaces .>>. name
            |>> (fun (cardTrait, name) -> GiveInformation (name, cardTrait))
    let parser = spaces >>. choice [addPlayer; startGame; giveInformation]
    match run parser input with
    | Success (command, _, _) -> command |> Result.Ok
    | Failure (errorMessage, _, _) -> Result.Error (sprintf "%A" errorMessage)

type private InputArtifact =
    | NewGame of EventHistory
    | CommandFailure of CannotPerformAction
    | InvalidCommand of string

let pipeline gameUpdated commandFailed inputInvalid inputStream =
    inputStream
    |> Observable.scan
        (fun (_, currentGameState) input ->
            input
            |> (parse >> Result.mapError InvalidCommand)
            |> Result.bind (Command.execute currentGameState >> Result.mapError CommandFailure)
            |> function
                | Result.Error errorArtifact -> (errorArtifact, currentGameState)
                | Result.Ok newGameState -> (NewGame newGameState, newGameState))
        (NewGame EventHistory.empty, EventHistory.empty)
    |> Observable.map fst
    |> Observable.subscribe (function
        | NewGame gameState -> gameUpdated gameState
        | CommandFailure reasons -> commandFailed reasons
        | InvalidCommand parseError -> inputInvalid parseError)
    |> List.singleton
