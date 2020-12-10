module Day6

open System
open FSharpPlus
open FSharpPlus.Math.Generic
open FSharpPlus.Data

module CombineGeneric =
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

module CombineStringList =
    let combineFn state row =
        let (previous, result) = state
        if row = ""
        then ([], previous :: result)
        else (row :: previous, result)

    let combineNonZero (rows : string list) =
        let (last, result) = List.fold combineFn ([], []) rows
        if List.length last = 0
        then result
        else last :: result

let toCharSet x = x |> toArray |> Set.ofArray

let countSame answers =
    answers
        |> List.map toCharSet
        |> List.reduce Set.intersect
        |> Set.count

let solve (rows : string list) =
    let a = rows
            |> List.map toCharSet
            |> CombineGeneric.combineNonZero
            |> List.map Set.count
            |> List.sum

    let b = rows
            |> CombineStringList.combineNonZero
            |> List.map countSame
            |> List.sum

    printfn "A=%i" a
    printfn "B=%i" b
    0
