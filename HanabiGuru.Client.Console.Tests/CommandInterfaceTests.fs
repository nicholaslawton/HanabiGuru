module HanabiGuru.Client.Console.Tests.CommandInterfaceTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Client.Console
open HanabiGuru.Engine.Tests

[<Property>]
let ``Command processing processes commands received and then terminates`` (commands : string list) =
    let mutable commandsQueue = commands
    let mutable commandsProcessed = []

    let processCommand command = commandsProcessed <- command :: commandsProcessed
    let pipeline stream = stream |> Observable.subscribe processCommand |> List.singleton

    let getInput () =
        match commandsQueue with
        | command :: remainingCommands ->
            commandsQueue <- remainingCommands
            Some command
        | [] -> None

    CommandInterface.processCommands getInput pipeline

    commandsQueue =! []
    List.rev commandsProcessed =! commands

let private contains contained (s : string) = s.Contains(contained)

let private errorMessage = function
    | Error message -> message
    | Ok _ -> new AssertionFailedException("Error expected") |> raise

[<Property>]
let ``Unrecognised input is rejected`` () =
    test <@ "unrecognised" |> CommandInterface.parseCommand |> errorMessage |> contains "Expecting: command" @>

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``Add player input can be parsed into command`` (ValidPlayerName name) =
    name |> sprintf "add player %s" |> CommandInterface.parseCommand =! (AddPlayer name |> Ok)

[<Property>]
let ``Add player input with no name is rejected`` () =
    test <@ "add player" |> CommandInterface.parseCommand |> errorMessage |> contains "Expecting: player name" @>
    test <@ "add player " |> CommandInterface.parseCommand |> errorMessage |> contains "Expecting: player name" @>
