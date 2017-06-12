module HanabiGuru.Client.Console.Tests.CommandInterfaceIntegrationTests

open Xunit
open Swensen.Unquote
open HanabiGuru.Client.Console
open HanabiGuru.Engine

[<Fact>]
let ``Commands are executed, failed commands are reported, and invalid input is rejected`` () =
    let input =
        [
            "sdfsld"
            "add player me"
            "asdfsae"
            "add player you"
            "add player me"
            "nnfjioi3e"
            "add player you"
        ]
    let mutable inputQueue = input

    let mutable gameUpdates = []
    let mutable failureReasons = []
    let mutable rejectedInput = []

    let getInput () =
        match inputQueue with
        | input :: remainingInput ->
            inputQueue <- remainingInput
            Some input
        | [] -> None

    let gameUpdated game = gameUpdates <- game :: gameUpdates
    let commandFailed reasons = failureReasons <- reasons :: failureReasons
    let inputInvalid input = rejectedInput <- input :: rejectedInput

    CommandInterface.processInput getInput (CommandInterface.pipeline gameUpdated commandFailed inputInvalid)

    List.length gameUpdates =! 2
    List.length rejectedInput =! 3
    failureReasons =! List.replicate 2 [PlayerAlreadyJoined]
