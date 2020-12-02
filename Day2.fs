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

let checkPolicy (policy, password) =
    let { Range=range; c=c} = policy
    let l = String.filter (fun b -> b = c) password |> String.length
    withinRange l range

let solve (rows : string list) =
    let a = rows |> List.collect runParseRow |> List.filter checkPolicy |> List.length
    printfn "A=%i" a
    0
