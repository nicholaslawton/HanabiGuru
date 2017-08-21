module HanabiGuru.Engine.Random

open System

let private rng = Random()

let int min max = rng.Next(min, max)

let double () = rng.NextDouble()
