namespace HanabiGuru.Engine.Tests

open System
open FsCheck
open HanabiGuru.Engine

type ValidPlayerName = ValidPlayerName of string
type Players = Players of Player list
type TwoPlayers = TwoPlayers of Player * Player
type CanAddPlayerArrangement = CanAddPlayerArrangement of Player * Player list
type TooManyPlayers = TooManyPlayers of Player * Player list
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
        |> Gen.map Player.create

    static member private listOfMinLength minLength =
        Gen.nonEmptyListOf 
        >> Gen.map List.distinct 
        >> Gen.filter (List.length >> ((<=) minLength)) 

    static member private listOfLengthBetween minLength maxLength =
        DistinctPlayers.listOfMinLength minLength
        >> Gen.filter (List.length >> ((>=) maxLength))

    static member private toArb arbType = Gen.map arbType >> Arb.fromGen

    static member ValidPlayerName() = DistinctPlayers.validName |> DistinctPlayers.toArb ValidPlayerName

    static member Players() =
        DistinctPlayers.validPlayer
        |> DistinctPlayers.listOfLengthBetween 0 Game.maximumPlayers
        |> DistinctPlayers.toArb Players

    static member TwoPlayers() = 
        DistinctPlayers.validPlayer
        |> Gen.two 
        |> Gen.filter (fun (x, y) -> x <> y)
        |> DistinctPlayers.toArb TwoPlayers

    static member CanAddPlayerArrangement() =
        DistinctPlayers.validPlayer
        |> DistinctPlayers.listOfLengthBetween 1 Game.maximumPlayers
        |> Gen.map (function
            | newPlayer :: seatedPlayers when List.length seatedPlayers < Game.maximumPlayers ->
                newPlayer, seatedPlayers
            | _ -> invalidOp (sprintf "Expecting at least one player and no more than %i" Game.maximumPlayers))
        |> DistinctPlayers.toArb CanAddPlayerArrangement
    
    static member TooManyPlayers() =
        DistinctPlayers.validPlayer
        |> DistinctPlayers.listOfMinLength (Game.maximumPlayers + 1)
        |> Gen.map (function
            | newPlayer :: seatedPlayers when List.length seatedPlayers >= Game.maximumPlayers ->
                newPlayer, seatedPlayers
            | _ -> invalidOp (sprintf "Expecting at least %i players" (Game.maximumPlayers + 1)))
        |> DistinctPlayers.toArb TooManyPlayers
    
    static member ValidPlayerView() =
        DistinctPlayers.validPlayer
        |> DistinctPlayers.listOfMinLength 2
        |> Gen.zip Arb.generate<PlayerView>
        |> Gen.map (fun (view, validPlayers) ->
            match validPlayers with
            | self :: others when not <| List.isEmpty others -> { view with self = self; otherPlayers = others }
            | _ -> invalidOp "Expecting at least two players")
        |> DistinctPlayers.toArb ValidPlayerView
