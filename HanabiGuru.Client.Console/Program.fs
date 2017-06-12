open System
open HanabiGuru.Client.Console

[<EntryPoint>]
let main _ = 
    let getInput () =
        match Console.ReadLine() with
        | "exit" -> None
        | line -> Some line
    CommandInterface.processInput getInput (CommandInterface.pipeline (printfn "%A") (printfn "%A") (printfn "%s"))

    0 // return an integer exit code
