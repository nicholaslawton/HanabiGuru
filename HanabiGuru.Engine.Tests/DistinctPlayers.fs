namespace HanabiGuru.Engine.Tests

open FsCheck
open HanabiGuru.Engine
open HanabiGuru.Engine.Model

type OneOrMorePlayers = Player * Player list 
type TwoPlayers = Player * Player
type TwoOrMorePlayers = Player list
type ValidPlayerView = PlayerView
 
type DistinctPlayers = 
    static member private playerWithNonEmptyName =
        Arb.generate<NonEmptyString> 
        |> Gen.map (fun name -> { name = name.Get })

    static member private listOfMinLength minLength =
        DistinctPlayers.playerWithNonEmptyName
        |> Gen.nonEmptyListOf 
        |> Gen.map List.distinct 
        |> Gen.filter (List.length >> ((<=) minLength)) 

    static member OneOrMorePlayers() = 
        DistinctPlayers.listOfMinLength 1
        |> Gen.map (function
            | one :: more -> one, more
            | [] -> invalidOp "Expecting at least one player")
        |> Arb.fromGen 

    static member TwoPlayers() = 
        DistinctPlayers.playerWithNonEmptyName
        |> Gen.two 
        |> Gen.filter (fun (x, y) -> x <> y)
        |> Arb.fromGen 
    
    static member TwoOrMorePlayers() =
        DistinctPlayers.listOfMinLength 2
        |> Arb.fromGen 
    
    static member ValidPlayerView() =
        DistinctPlayers.listOfMinLength 2
        |> Gen.map (function
            | self :: others -> { self = self; otherPlayers = others }
            | [] -> invalidOp "Expecting at least one player")
        |> Arb.fromGen
