module Day9

open System
open FParsec

let rec solveA rows =
    let allowed = List.take 25 rows
    let expectedSum = rows.[25]
    let rec innerCheck remaining =
        match remaining with
        | h :: t ->
            let other = expectedSum - h
            if other > 0L && List.exists (fun x -> x = other) t
            then true
            else innerCheck t
        | _ -> false
    if innerCheck allowed
    then solveA (List.tail rows)
    else expectedSum

let rec solveB target rows =
    let rec checkWeakness sum mx numbers =
        match numbers with
        | h :: t ->
            let nextMx = max h mx
            let next = sum + h
            if next = target then Some nextMx
            elif next > target then None
            else checkWeakness next nextMx t
        | _ -> None
    match rows with
    | h :: t -> match (checkWeakness h h t) with
                | Some x -> [h, x, h + x]
                | None -> solveB target t
    | _ -> []

let runParse parser s =
    match run parser s with
    | Success(result, _, _)   -> Some result
    | Failure(errorMsg, _, _) -> None

let solve (rows : string list) =
    let parsed = List.choose (runParse pint64) rows
    let a = solveA parsed
    printfn "A=%A" a
    let b = solveB a parsed
    printfn "B=%A" b
    0
