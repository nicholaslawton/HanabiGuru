namespace HanabiGuru.Engine.Tests

open System
open FsCheck
open HanabiGuru.Engine

type PlayerName = PlayerName of string
type PlayerNames = PlayerNames of string list
type CompletePlayerNames = CompletePlayerNames of string list
type Players = Players of PlayerIdentity list
type TwoPlayers = TwoPlayers of PlayerIdentity * PlayerIdentity
type CanAddPlayerArrangement = CanAddPlayerArrangement of PlayerIdentity * PlayerIdentity list
type TooManyPlayers = TooManyPlayers of PlayerIdentity * PlayerIdentity list
type ValidPlayerView = ValidPlayerView of PlayerView

type DistinctPlayers = 
    static member private validName =
        Arb.generate<char>
        |> Gen.filter (fun c -> c >= ' ')
        |> Gen.nonEmptyListOf
        |> Gen.map String.Concat
        |> Gen.map (fun name -> name.Trim())
        |> Gen.filter (fun name -> not <| String.IsNullOrWhiteSpace(name))

    static member private validPlayer =
        DistinctPlayers.validName
        |> Gen.map PlayerIdentity.create

    static member private listOfMinLength minLength =
        Gen.nonEmptyListOf 
        >> Gen.map List.distinct 
        >> Gen.filter (List.length >> ((<=) minLength)) 

    static member private listOfLengthBetween minLength maxLength =
        DistinctPlayers.listOfMinLength minLength
        >> Gen.filter (List.length >> ((>=) maxLength))

    static member private listOfMaxLength maxLength = DistinctPlayers.listOfLengthBetween 0 maxLength

    static member private toArb arbType = Gen.map arbType >> Arb.fromGen

    static member PlayerName() = DistinctPlayers.validName |> DistinctPlayers.toArb PlayerName

    static member PlayerNames() =
        DistinctPlayers.validName
        |> DistinctPlayers.listOfMaxLength GameRules.maximumPlayers
        |> DistinctPlayers.toArb PlayerNames

    static member CompletePlayerNames() =
        DistinctPlayers.validName
        |> DistinctPlayers.listOfLengthBetween GameRules.minimumPlayers GameRules.maximumPlayers
        |> DistinctPlayers.toArb CompletePlayerNames

    static member Players() =
        DistinctPlayers.validPlayer
        |> DistinctPlayers.listOfMaxLength GameRules.maximumPlayers
        |> DistinctPlayers.toArb Players

    static member TwoPlayers() = 
        DistinctPlayers.validPlayer
        |> Gen.two 
        |> Gen.filter (fun (x, y) -> x <> y)
        |> DistinctPlayers.toArb TwoPlayers

    static member CanAddPlayerArrangement() =
        DistinctPlayers.validPlayer
        |> DistinctPlayers.listOfLengthBetween 1 GameRules.maximumPlayers
        |> Gen.map (function
            | newPlayer :: seatedPlayers when List.length seatedPlayers < GameRules.maximumPlayers ->
                newPlayer, seatedPlayers
            | _ -> invalidOp (sprintf "Expecting at least one player and no more than %i" GameRules.maximumPlayers))
        |> DistinctPlayers.toArb CanAddPlayerArrangement
    
    static member TooManyPlayers() =
        DistinctPlayers.validPlayer
        |> DistinctPlayers.listOfMinLength (GameRules.maximumPlayers + 1)
        |> Gen.map (function
            | newPlayer :: seatedPlayers when List.length seatedPlayers >= GameRules.maximumPlayers ->
                newPlayer, seatedPlayers
            | _ -> invalidOp (sprintf "Expecting at least %i players" (GameRules.maximumPlayers + 1)))
        |> DistinctPlayers.toArb TooManyPlayers
    
    static member ValidPlayerView() =
        DistinctPlayers.validPlayer
        |> DistinctPlayers.listOfMinLength 2
        |> Gen.zip Arb.generate<PlayerView>
        |> Gen.map (fun (view, validPlayers) ->
            match validPlayers with
            | self :: others when not <| List.isEmpty others ->
                { view with
                    self = PlayerState.create self
                    otherPlayers = List.map PlayerState.create others
                }
            | _ -> invalidOp "Expecting at least two players")
        |> DistinctPlayers.toArb ValidPlayerView
