open System
open HanabiGuru.Client.Console

[<EntryPoint>]
let main _ = 
    let pipeline = Observable.subscribe(printfn "%s")
    let getInput () =
        match Console.ReadLine() with
        | "exit" -> None
        | line -> Some line
    CommandInterface.processCommands getInput pipeline
    0 // return an integer exit code
