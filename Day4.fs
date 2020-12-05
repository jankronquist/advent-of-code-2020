module Day4

open System
open FParsec

module Map =
    module private Kvp =

        open System.Collections.Generic

        let toTuple (kvp : KeyValuePair<_,_>) =
            kvp.Key, kvp.Value

    /// Concatenate all the input maps. Where there are duplicate input keys,
    /// the last mapping is preserved.
    let concat maps =
        maps
        |> Seq.concat
        |> Seq.map Kvp.toTuple
        |> Map.ofSeq


// byr (Birth Year)
// iyr (Issue Year)
// eyr (Expiration Year)
// hgt (Height)
// hcl (Hair Color)
// ecl (Eye Color)
// pid (Passport ID)
// cid (Country ID)

let parseKeyValue = tuple2 (regex "\\w+") (pstring ":" >>. (regex "(\\w|#)+") .>> spaces)

let parseItem = many parseKeyValue |>> Map.ofList

let runParseRow row =
    match run parseItem row with
    | Success(result, _, _)   -> [result]
    | Failure(errorMsg, _, _) -> []

let rec combineRows previous rows =
    match rows with
    | [] -> if Map.isEmpty previous
            then []
            else [previous]
    | (a :: rest) ->
        if Map.isEmpty a
        then
            if Map.isEmpty previous
            then combineRows previous rest
            else previous :: (combineRows Map.empty rest)
        else
            combineRows (Map.concat [|previous; a|]) rest

let isValidPassport m =
    // let c = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] |> List.map Map.containsKey |> List.filter (fun fn -> fn m) |> List.length
    // c = 7
    Map.containsKey "byr" m &&
        Map.containsKey "iyr" m &&
        Map.containsKey "eyr" m &&
        Map.containsKey "hgt" m &&
        Map.containsKey "hcl" m &&
        Map.containsKey "ecl" m &&
        Map.containsKey "pid" m

let solve (rows : string list) =
    let parsed = rows |> List.collect runParseRow

    let combined = combineRows Map.empty parsed
    printfn "test=%A" combined

    let a = combined |> List.filter isValidPassport |> List.length

    printfn "A=%i" a
    0
