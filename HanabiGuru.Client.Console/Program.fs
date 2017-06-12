open System
open HanabiGuru.Client.Console
open HanabiGuru.Engine

[<EntryPoint>]
let main _ = 
    let getInput () =
        match Console.ReadLine() with
        | "exit" -> None
        | line -> Some line
    CommandInterface.processCommands getInput (CommandInterface.pipeline (printfn "%A") (printfn "%s"))

    0 // return an integer exit code
