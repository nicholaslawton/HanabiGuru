module HanabiGuru.Client.Console.Tests.CommandInterfaceTests

open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Client.Console
open HanabiGuru.Engine

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
    test <@ "unrecognised" |> CommandInterface.parse |> errorMessage |> contains "Expecting:" @>

[<Property(Arbitrary = [| typeof<InputGeneration> |])>]
let ``Add player input is parsed into command`` (ValidName name) =
    name |> sprintf "add player %s" |> CommandInterface.parse =! (AddPlayer name |> Ok)

[<Fact>]
let ``Add player input with no name is rejected`` () =
    test <@ "add player" |> CommandInterface.parse |> errorMessage |> contains "Expecting: player name" @>
    test <@ "add player " |> CommandInterface.parse |> errorMessage |> contains "Expecting: player name" @>

[<Fact>]
let ``Start game input is parsed into command`` () =
    "start" |> CommandInterface.parse =! Ok StartGame

let traitString = function
    | SuitTrait suit -> sprintf "%A" suit
    | RankTrait (Rank rank) -> sprintf "%A" rank

[<Property(Arbitrary = [| typeof<InputGeneration> |])>]
let ``Give information input is parsed into command`` (ValidName name) (cardTrait : CardTrait) =
    sprintf "tell %s %s" (traitString cardTrait) name
    |> CommandInterface.parse =! Ok (GiveInformation (name, cardTrait))

[<Fact>]
let ``Give information input with no details is rejected`` () =
    test <@ "tell" |> CommandInterface.parse |> errorMessage |> contains "Expecting: colour or number" @>
    test <@ "tell " |> CommandInterface.parse |> errorMessage |> contains "Expecting: colour or number" @>

[<Fact>]
let ``Give information input with invalid colour or number is rejected`` () =
    test <@ "tell invalid" |> CommandInterface.parse |> errorMessage |> contains "Expecting: colour or number" @>

[<Property>]
let ``Give information input with no name is rejected`` (cardTrait : CardTrait) =
    test <@ sprintf "tell %s" (traitString cardTrait)
        |> CommandInterface.parse
        |> errorMessage
        |> contains "Expecting: player name" @>
    test <@ sprintf "tell %s " (traitString cardTrait)
        |> CommandInterface.parse
        |> errorMessage
        |> contains "Expecting: player name" @>

[<Property>]
let ``Discard card input is parsed into command`` (c : char) =
    sprintf "%c" c |> String.filter (not << System.Char.IsWhiteSpace) |> String.length > 0 ==> lazy
    (sprintf "discard %c" c |> CommandInterface.parse =! Ok (DiscardCard c)) 

[<Fact>]
let ``Discard card input with no card id is rejected`` () =
    test <@ "discard" |> CommandInterface.parse |> errorMessage |> contains "Expecting: card identifier" @>
