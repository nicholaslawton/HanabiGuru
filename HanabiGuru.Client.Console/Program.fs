open System
open HanabiGuru.Client.Console

[<EntryPoint>]
let main _ = 
    let pipeline =
        Observable.map (CommandInterface.parseCommand)
        >> Observable.split (function
            | Ok command -> Choice1Of2 command
            | Error message -> Choice2Of2 message)
        >> fun (commands, errors) ->
            [
                commands |> Observable.subscribe (printfn "Execute: %A")
                errors |> Observable.subscribe (printfn "Error: %A")
            ]
    let getInput () =
        match Console.ReadLine() with
        | "exit" -> None
        | line -> Some line
    CommandInterface.processCommands getInput pipeline

    0 // return an integer exit code
