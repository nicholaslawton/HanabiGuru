module HanabiGuru.Client.Console.Tests.CommandInterfaceTests

open Xunit
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Client.Console
open HanabiGuru.Engine.Tests

[<Property>]
let ``Input processing processes input received and then terminates`` (input : string list) =
    let mutable inputQueue = input
    let mutable inputProcessed = []

    let processInput input = inputProcessed <- input :: inputProcessed
    let pipeline stream = stream |> Observable.subscribe processInput |> List.singleton

    let getInput () =
        match inputQueue with
        | input :: remainingInput ->
            inputQueue <- remainingInput
            Some input
        | [] -> None

    CommandInterface.processInput getInput pipeline

    inputQueue =! []
    List.rev inputProcessed =! input

let private contains contained (s : string) = s.Contains(contained)

let private errorMessage = function
    | Error message -> message
    | Ok _ -> new AssertionFailedException("Error expected") |> raise

[<Fact>]
let ``Unrecognised input is rejected`` () =
    test <@ "unrecognised" |> CommandInterface.parse |> errorMessage |> contains "Expecting: command" @>

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``Add player input can be parsed into command`` (ValidPlayerName name) =
    name |> sprintf "add player %s" |> CommandInterface.parse =! (AddPlayer name |> Ok)

[<Fact>]
let ``Add player input with no name is rejected`` () =
    test <@ "add player" |> CommandInterface.parse |> errorMessage |> contains "Expecting: player name" @>
    test <@ "add player " |> CommandInterface.parse |> errorMessage |> contains "Expecting: player name" @>
