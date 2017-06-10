module HanabiGuru.Client.Console.CommandInterface

type private EventStream<'a> () = 

    let events = new Event<'a>() 
 
    [<CLIEvent>] 
    member this.EventStream = events.Publish 

    member this.RaiseEvent event = events.Trigger event

let processCommands getInput pipeline =
    let stream = EventStream ()
    use subscription = stream.EventStream |> pipeline
    let rec processNextCommand () =
        getInput ()
        |> Option.iter (fun command ->
            stream.RaiseEvent command
            processNextCommand () |> ignore)
    processNextCommand ()
