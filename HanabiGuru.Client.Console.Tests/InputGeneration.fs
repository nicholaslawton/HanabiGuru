namespace HanabiGuru.Client.Console.Tests

open System
open FsCheck
open HanabiGuru.Engine

type ValidName = ValidName of string
type ValidNames = ValidNames of Set<string>

type InputGeneration =
    static member private nameGenerator =
        Arb.generate<char>
        |> Gen.filter (fun c -> ' ' <= c && c <= '~')
        |> Gen.nonEmptyListOf
        |> Gen.map String.Concat
        |> Gen.map (fun s -> s.Trim())
        |> Gen.filter (not << String.IsNullOrWhiteSpace)

    static member ValidName() =
        InputGeneration.nameGenerator
        |> Gen.map ValidName
        |> Arb.fromGen

    static member ValidNames() =
        InputGeneration.nameGenerator
        |> Gen.nonEmptyListOf
        |> Gen.map set
        |> Gen.filter (Set.count >> ((<=) GameRules.minimumPlayers)) 
        |> Gen.filter (Set.count >> ((>=) GameRules.maximumPlayers)) 
        |> Gen.map ValidNames
        |> Arb.fromGen
