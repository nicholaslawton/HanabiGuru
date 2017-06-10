namespace HanabiGuru.Engine.Tests

open System
open FsCheck
open HanabiGuru.Engine
open HanabiGuru.Engine.Model

type ValidPlayerName = ValidPlayerName of string
type Players = Players of Player list
type OneOrMorePlayers = OneOrMorePlayers of Player * Player list 
type TwoPlayers = TwoPlayers of Player * Player
type TwoOrMorePlayers = TwoOrMorePlayers of Player list
type CanAddPlayerArrangement = CanAddPlayerArrangement of Player * Player list
type TooManyPlayers = TooManyPlayers of Player * Player list
type ValidPlayerView = ValidPlayerView of PlayerView

type DistinctPlayers = 
    static member private validName =
        Arb.generate<NonEmptyString> 
        |> Gen.filter (fun name -> not <| String.IsNullOrWhiteSpace(name.Get))
        |> Gen.map (fun name -> name.Get.Trim().Replace("\n", "").Replace("\r", ""))

    static member private validPlayer =
        DistinctPlayers.validName
        |> Gen.map (fun name -> { name = name })

    static member private listOfMinLength minLength =
        DistinctPlayers.validPlayer
        |> Gen.nonEmptyListOf 
        |> Gen.map List.distinct 
        |> Gen.filter (List.length >> ((<=) minLength)) 

    static member private listOfLengthBetween minLength maxLength =
        DistinctPlayers.listOfMinLength minLength
        |> Gen.filter (List.length >> ((>=) maxLength))

    static member private toArb arbType = Gen.map arbType >> Arb.fromGen

    static member ValidPlayerName() = DistinctPlayers.validName |> DistinctPlayers.toArb ValidPlayerName
    static member Players() = DistinctPlayers.listOfLengthBetween 0 Game.playerLimit |> DistinctPlayers.toArb Players

    static member OneOrMorePlayers() = 
        DistinctPlayers.listOfLengthBetween 1 Game.playerLimit
        |> Gen.map (function
            | one :: more -> one, more
            | [] -> invalidOp "Expecting at least one player")
        |> DistinctPlayers.toArb OneOrMorePlayers

    static member TwoPlayers() = 
        DistinctPlayers.validPlayer
        |> Gen.two 
        |> Gen.filter (fun (x, y) -> x <> y)
        |> DistinctPlayers.toArb TwoPlayers
    
    static member TwoOrMorePlayers() =
        DistinctPlayers.listOfLengthBetween 2 Game.playerLimit
        |> DistinctPlayers.toArb TwoOrMorePlayers
    
    static member CanAddPlayerArrangement() =
        DistinctPlayers.listOfLengthBetween 1 Game.playerLimit
        |> Gen.map (function
            | newPlayer :: seatedPlayers when List.length seatedPlayers < Game.playerLimit ->
                newPlayer, seatedPlayers
            | _ -> invalidOp (sprintf "Expecting at least one player and no more than %i" Game.playerLimit))
        |> DistinctPlayers.toArb CanAddPlayerArrangement
    
    static member TooManyPlayers() =
        DistinctPlayers.listOfMinLength (Game.playerLimit + 1)
        |> Gen.map (function
            | newPlayer :: seatedPlayers when List.length seatedPlayers >= Game.playerLimit ->
                newPlayer, seatedPlayers
            | _ -> invalidOp (sprintf "Expecting at least %i players" (Game.playerLimit + 1)))
        |> DistinctPlayers.toArb TooManyPlayers
    
    static member ValidPlayerView() =
        DistinctPlayers.listOfMinLength 2
        |> Gen.map (function
            | self :: others -> { self = self; otherPlayers = others }
            | [] -> invalidOp "Expecting at least two players")
        |> DistinctPlayers.toArb ValidPlayerView
