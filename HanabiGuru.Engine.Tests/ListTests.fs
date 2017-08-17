module HanabiGuru.Engine.Tests.ListTests

open Xunit
open FsCheck.Xunit
open Swensen.Unquote
open System
open HanabiGuru.Engine

let assertSortingBeforeOperationSameAsAfter f list =
    let fThenSort = f >> List.sort
    let sortThenF = List.sort >> f
    fThenSort list =! sortThenF list

[<Property>]
let ``Extracting an item from an empty list returns nothing and an empty list`` (x : string) =
    List.extract x List.empty =! (None, List.empty)

[<Property>]
let ``Extracting the first item from a list returns the item and the tail`` (head : int) (tail : int list) =
    (head :: tail) |> List.extract head =! (Some head, tail)

[<Property>]
let ``Removal is commutative`` (list : int list) (x : int) (y : int) =
    list |> List.remove x |> List.remove y =! (list |> List.remove y |> List.remove x)

[<Property>]
let ``Removing and then sorting leaves the same list as sorting and then removing`` (list : char list) (x : char) =
    assertSortingBeforeOperationSameAsAfter (List.remove x) list

[<Property>]
let ``Removing from an empty list returns an empty list`` (list : char list) =
    List.removeEach list List.empty =! List.empty

[<Property>]
let ``Removing an empty list from a list returns the list`` (list : string list) =
    List.removeEach List.empty list =! list

[<Property>]
let ``Removing a list from itself returns an empty list`` (list : int list) =
    List.removeEach list list =! List.empty

[<Property>]
let ``List removal is commutative`` (list : int list) (xs : int list) (ys : int list) =
    list |> List.removeEach xs |> List.removeEach ys =! (list |> List.removeEach ys |> List.removeEach xs)

[<Property>]
let ``Removing and then sorting is the same as sorting and then removing`` (list : int list) (xs : int list) =
    assertSortingBeforeOperationSameAsAfter (List.removeEach xs) list

[<Property>]
let ``Remove followed by except is the same as except followed by remove`` (list : int list) (xs : int list) =
    let removeThenExcept xs = List.removeEach xs >> List.except xs
    let exceptThenRemove xs = List.except xs >> List.removeEach xs
    removeThenExcept xs list =! exceptThenRemove xs list

[<Property>]
let ``Remove followed by except is the same as except`` (list : int list) (xs : int list) =
    let removeThenExcept xs = List.removeEach xs >> List.except xs
    removeThenExcept xs list =! List.except xs list

[<Property>]
let ``Removing the first part of a list returns the rest of the list`` (firstPart : int list) (rest : int list) =
    (firstPart @ rest) |> List.removeEach firstPart =! rest

[<Property>]
let ``List removal returns a list reduced by the number of overlapping items`` (list : int list) (xs : int list) =
    let expectedLength =
        List.map (fun value -> (value, 1)) list
        |> List.append (List.map (fun value -> (value, -1)) xs)
        |> List.groupBy fst
        |> List.map (fun (value, occurrences) -> (value, List.sumBy snd occurrences |> max 0))
        |> List.sumBy snd
    List.removeEach xs list |> List.length =! expectedLength

[<Fact>]
let ``Selecting a random item from a list fails with an empty list`` () =
    raises<ArgumentException> <@ List.randomItem Random.int [] @>

[<Fact>]
let ``Selecting a random item from a list does not return the same item each time`` () =
    [0..10]
    |> List.replicate 100
    |> List.map (List.randomItem Random.int)
    |> List.countBy id
    |> List.filter (snd >> ((<) 30)) =! []

[<Property>]
let ``Splitting a list and then collecting is the same as filtering`` (list : int list) (predicate : int -> bool) =
    List.splitBy predicate list |> List.collect id =! List.filter (not << predicate) list

[<Property>]
let ``Splitting a list containing no items matching the predicate returns a list containing the input list``
    (list : int list) =

    List.splitBy (fun _ -> false) list =! [list]

[<Property>]
let ``Splitting a list returns a number of lists equal to one more than the number of items matching the predicate``
    (list : int list)
    (predicate : int -> bool) =
    test <@ List.splitBy predicate list |> List.length = ((list |> List.filter predicate |> List.length) + 1) @>

[<Property>]
let ``Weaving a list of length less than 2 returns the input list unmodified`` (item : string option) (weave : string) =
    let list = Option.toList item
    List.weave weave list =! list

[<Property>]
let ``Weaving a list increases the length of the list by one less than its length``
    ((first, second, additional) : double * double * double list)
    (weave : double) =

    let list = first :: second :: additional
    List.weave weave list |> List.length =! List.length list * 2 - 1

[<Property>]
let ``All odd numbered elements in a weaved list are the weaved item``
    ((first, second, additional) : int * int * int list)
    (weave : int) =

    let list = first :: second :: additional
    let isOdd x = x % 2 = 1
    List.weave weave list
    |> List.indexed
    |> List.filter (fst >> isOdd)
    |> List.map snd
    |> List.distinct
    |> List.exactlyOne =! weave
