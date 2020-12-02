module Day2

open System
open FParsec

type Range = {Start:int; End:int}

let range s e = { Start=s; End=e }

let parseRange = pipe2 (pint32 .>> pstring "-") (pint32 .>> pstring " ") range

type Policy = {Range: Range; c: char}

let policy r c = { Range=r; c=c }

let parsePolicy = pipe2 parseRange (anyChar .>> pstring ": ") policy

let parseRow = tuple2 parsePolicy (restOfLine true)

let runParseRow row =
    match run parseRow row with
    | Success(result, _, _)   -> [result]
    | Failure(errorMsg, _, _) -> []

let withinRange v { Start=s; End=e } = v >= s && v <= e

let checkPolicyA (policy, password) =
    let { Range=range; c=c} = policy
    let l = String.filter (fun b -> b = c) password |> String.length
    withinRange l range

let checkPolicyB (policy, password : string) =
    let { Range={ Start=s; End=e }; c=c} = policy
    password.[s-1] = c && password.[e-1] <> c || password.[s-1] <> c && password.[e-1] = c

let solve (rows : string list) =
    let parsed = rows |> List.collect runParseRow
    let a = parsed |> List.filter checkPolicyA |> List.length
    let b = parsed |> List.filter checkPolicyB |> List.length
    printfn "A=%i" a
    printfn "B=%i" b
    0
