module Day1

open System

let add x y = x + y

let rec solveA s numbers : int Option =
    match numbers with
    | [] -> None
    | (a :: rest) -> match List.filter (fun b -> a + b = s) rest with
                        | [] -> solveA s rest
                        | (b :: r) -> Some (a * b)

let rec solveB s numbers =
    match numbers with
    | [] -> None
    | (a :: rest) -> match solveA (s - a) rest with
                     | Some c -> Some (a * c)
                     | _ -> solveB s rest

let solve (rows : string list) =
    let parsed = List.map int rows
    let a = solveA 2020 parsed
    printfn "A=%i" (Option.get a)
    let b = solveB 2020 parsed
    printfn "B=%i" (Option.get b)
    0
