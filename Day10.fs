module Day10

open System
open FParsec

let solveA (previous, s1, s3) next =
    match (next - previous) with
    | 1 -> (next, s1 + 1, s3)
    | 3 -> (next, s1, s3 + 1)
    | diff ->
        printfn "next=%i diff=%i" next diff
        (next, s1, s3)

let solve (rows : string list) =
    let parsed = List.map int rows
    let sorted = List.sort parsed
    let (_, s1, s3) = List.fold solveA (0, 0, 0) sorted
    let a = s1 * (s3 + 1)

    // let a = solveA parsed
    printfn "A=%A" a
    0
