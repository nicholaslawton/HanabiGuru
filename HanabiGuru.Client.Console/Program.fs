open System
open HanabiGuru.Client.Console

[<EntryPoint>]
let main _ = 
    let getInput () =
        printf "> "
        match Console.ReadLine() with
        | "exit" -> None
        | line -> Some line
    CommandInterface.pipeline Presentation.game Presentation.commandFailure Presentation.invalidInput
    |> CommandInterface.processInput getInput

    0 // return an integer exit code
