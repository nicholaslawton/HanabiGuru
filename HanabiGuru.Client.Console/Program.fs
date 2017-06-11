open System
open HanabiGuru.Client.Console
open HanabiGuru.Engine

[<EntryPoint>]
let main _ = 
    let pipeline =
        Observable.map (CommandInterface.parseCommand)
        >> Observable.split (function
            | Ok command -> Choice1Of2 command
            | Error message -> Choice2Of2 message)
        >> fun (commands, errors) ->
            [
                errors |> Observable.subscribe (printfn "%A")
                commands
                |> Observable.scan (Commands.execute ignore) EventHistory.empty
                |> Observable.map (EventHistory.allEvents)
                |> Observable.map (List.fold GameData.processEvent GameData.initial)
                |> Observable.subscribe (printfn "%A")
            ]
    let getInput () =
        match Console.ReadLine() with
        | "exit" -> None
        | line -> Some line
    CommandInterface.processCommands getInput pipeline

    0 // return an integer exit code
