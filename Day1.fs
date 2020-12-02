module Day1

open System

let add x y = x + y

let rec solveA = function
    | [] -> -1
    | (a :: rest) -> match List.filter (fun b -> a + b = 2020) rest with
                        | [] -> solveA rest
                        | (b :: r) -> (a * b)

let solve (rows : string list) =
    let parsed = List.map int rows
    printfn "A=%i" (solveA parsed)
    0
