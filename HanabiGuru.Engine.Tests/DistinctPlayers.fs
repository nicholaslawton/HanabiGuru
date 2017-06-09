namespace HanabiGuru.Engine.Tests

open FsCheck
open HanabiGuru.Engine
open HanabiGuru.Engine.Model

type Players = Players of Player list
type OneOrMorePlayers = OneOrMorePlayers of Player * Player list 
type TwoPlayers = TwoPlayers of Player * Player
type TwoOrMorePlayers = TwoOrMorePlayers of Player list
type CanAddPlayerArrangement = CanAddPlayerArrangement of Player * Player list
type TooManyPlayers = TooManyPlayers of Player * Player list
type ValidPlayerView = ValidPlayerView of PlayerView
 
type DistinctPlayers = 
    static member private playerWithNonEmptyName =
        Arb.generate<NonEmptyString> 
        |> Gen.map (fun name -> { name = name.Get })

    static member private listOfMinLength minLength =
        DistinctPlayers.playerWithNonEmptyName
        |> Gen.nonEmptyListOf 
        |> Gen.map List.distinct 
        |> Gen.filter (List.length >> ((<=) minLength)) 

    static member Players() = DistinctPlayers.listOfMinLength 0 |> Gen.map Players |> Arb.fromGen

    static member OneOrMorePlayers() = 
        DistinctPlayers.listOfMinLength 1
        |> Gen.map (function
            | one :: more -> one, more
            | [] -> invalidOp "Expecting at least one player")
        |> Gen.map OneOrMorePlayers
        |> Arb.fromGen

    static member TwoPlayers() = 
        DistinctPlayers.playerWithNonEmptyName
        |> Gen.two 
        |> Gen.filter (fun (x, y) -> x <> y)
        |> Gen.map TwoPlayers
        |> Arb.fromGen 
    
    static member TwoOrMorePlayers() = DistinctPlayers.listOfMinLength 2 |> Gen.map TwoOrMorePlayers |> Arb.fromGen
    
    static member CanAddPlayerArrangement() =
        DistinctPlayers.listOfMinLength 1
        |> Gen.filter (List.length >> ((>=) Game.playerLimit))
        |> Gen.map (function
            | newPlayer :: seatedPlayers when List.length seatedPlayers < Game.playerLimit ->
                newPlayer, seatedPlayers
            | _ -> invalidOp (sprintf "Expecting at least one player and no more than %i" Game.playerLimit))
        |> Gen.map CanAddPlayerArrangement
        |> Arb.fromGen 
    
    static member TooManyPlayers() =
        DistinctPlayers.listOfMinLength (Game.playerLimit + 1)
        |> Gen.map (function
            | newPlayer :: seatedPlayers when List.length seatedPlayers >= Game.playerLimit ->
                newPlayer, seatedPlayers
            | _ -> invalidOp (sprintf "Expecting at least %i players" (Game.playerLimit + 1)))
        |> Gen.map TooManyPlayers
        |> Arb.fromGen 
    
    static member ValidPlayerView() =
        DistinctPlayers.listOfMinLength 2
        |> Gen.map (function
            | self :: others -> { self = self; otherPlayers = others }
            | [] -> invalidOp "Expecting at least two players")
        |> Gen.map ValidPlayerView
        |> Arb.fromGen
