module HanabiGuru.Client.Console.Tests.CommandInterfaceIntegrationTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Client.Console
open HanabiGuru.Engine

[<Property>]
let ``Commands are exected, failed commands are reported, and invalid input is rejected`` () =
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
    let mutable rejectedInput = []
    let mutable failureReasons = []

    let getInput () =
        match inputQueue with
        | input :: remainingInput ->
            inputQueue <- remainingInput
            Some input
        | [] -> None

    let handleFailedCommand reasons = failureReasons <- reasons :: failureReasons
    let handleInvalidInput input = rejectedInput <- input :: rejectedInput

    CommandInterface.processCommands getInput (CommandInterface.pipeline handleFailedCommand handleInvalidInput)

    List.length rejectedInput =! 3
    failureReasons =! [[PlayerAlreadyJoined]; [PlayerAlreadyJoined]]
