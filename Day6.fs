module Day6

open System
open FSharpPlus
open FSharpPlus.Math.Generic
open FSharpPlus.Data

let combineFn state row =
    let (previous, result) = state
    if row = zero
    then (zero, previous :: result)
    else (previous + row, result)

let combineNonZero rows =
    let (last, result) = List.fold combineFn (zero, []) rows
    if last = zero
    then result
    else last :: result

let solve (rows : string list) =
    let a = rows
            |> List.map (fun x -> x |> toArray |> Set.ofArray)
            |> combineNonZero
            |> List.map Set.count
            |> List.sum

    printfn "A=%i" a
    // printfn "B=%i" b
    0
