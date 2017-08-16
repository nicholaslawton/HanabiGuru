namespace HanabiGuru.Client.Console.Tests

open System
open FsCheck
open HanabiGuru.Engine

type Name = Name of string
type Names = Names of Set<string>

type InputGeneration =
    static member private removeNewLines (NonEmptyString s) =
        s |> Seq.toList |> List.removeEach (Seq.toList Environment.NewLine) |> string

    static member Name() =
        Arb.generate<NonEmptyString>
        |> Gen.map InputGeneration.removeNewLines
        |> Gen.filter (not << String.IsNullOrWhiteSpace)
        |> Gen.map Name
        |> Arb.fromGen

    static member Names() =
        Arb.generate<Set<NonEmptyString>>
        |> Gen.map (Set.map InputGeneration.removeNewLines)
        |> Gen.map (Set.filter (not << String.IsNullOrWhiteSpace))
        |> Gen.filter (Set.count >> ((<=) GameRules.minimumPlayers)) 
        |> Gen.filter (Set.count >> ((>=) GameRules.maximumPlayers)) 
        |> Gen.map Names
        |> Arb.fromGen
