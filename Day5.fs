module Day5

open System

let fromSillyBinary c =
    match c with
    | 'F' -> '0'
    | 'B' -> '1'
    | 'R' -> '1'
    | 'L' -> '0'
    | _ -> '?'

let sillyBinaryToInt c = Convert.ToInt32(String.map fromSillyBinary c, 2);

let solve (rows : string list) =
    let parsed = List.map sillyBinaryToInt rows

    let a = List.max parsed

    // let a = Convert.ToInt32(String.map fromSillyBinary "FBFBBFFRLR", 2);

    printfn "A=%i" a
    // printfn "B=%i" b
    0
