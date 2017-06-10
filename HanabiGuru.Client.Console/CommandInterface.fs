module HanabiGuru.Client.Console.CommandInterface

let processCommands getInput pipeline =
    let events = new Event<_> ()
    let rec processNextInput () =
        getInput ()
        |> Option.iter (fun input ->
            events.Trigger input
            processNextInput () |> ignore)
    use subscription = events.Publish |> pipeline
    processNextInput ()
