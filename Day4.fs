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

let parseEcl = (pstring "amb" <|> pstring "blu" <|> pstring "brn" <|> pstring "gry" <|> pstring "grn" <|> pstring "hzl" <|> pstring "oth") .>> eof

let parseHcl = pstring "#" >>. many1 hex .>> eof
let validHcl l = List.length l = 6

let parsePid = many1 digit .>> eof
let validPid l = List.length l = 9

type Length = {Amount:int32; Unit:string}
let makeHgt l u = {Amount=l; Unit=u}
let parseHgt = pipe2 pint32 ((pstring "cm" <|> pstring "in") .>> eof) makeHgt

let validHgt {Amount=a; Unit=u} =
    if (u = "cm")
    then
        a >= 150 && a <= 193
    else
        a >= 59 && a <= 76

let runParse s parser =
    match run parser s with
    | Success(result, _, _)   -> Some result
    | Failure(errorMsg, _, _) -> None

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
    Map.containsKey "byr" m &&
        Map.containsKey "iyr" m &&
        Map.containsKey "eyr" m &&
        Map.containsKey "hgt" m &&
        Map.containsKey "hcl" m &&
        Map.containsKey "ecl" m &&
        Map.containsKey "pid" m

let hasLength l s =
    l = String.length s

let isLowercase (s : string) =
    s = s.ToLower()

// byr (Birth Year) - four digits; at least 1920 and at most 2002.
// iyr (Issue Year) - four digits; at least 2010 and at most 2020.
// eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
// hgt (Height) - a number followed by either cm or in:
// If cm, the number must be at least 150 and at most 193.
// If in, the number must be at least 59 and at most 76.
// hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
// ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
// pid (Passport ID) - a nine-digit number, including leading zeroes.
// cid (Country ID) - ignored, missing or not.
let isReallyValid m =
    let byr = int (Map.find "byr" m)
    let iyr = int (Map.find "iyr" m)
    let eyr = int (Map.find "eyr" m)
    byr >= 1920 && byr <= 2002
        && iyr >= 2010 && iyr <= 2020
        && eyr >= 2020 && iyr <= 2030
        && runParse (Map.find "hgt" m) parseHgt |> Option.filter validHgt |> Option.isSome
        && runParse (Map.find "hcl" m) parseHcl |> Option.filter validHcl |> Option.isSome
        && runParse (Map.find "ecl" m) parseEcl |> Option.isSome
        && runParse (Map.find "pid" m) parsePid |> Option.filter validPid |> Option.isSome
        && (Map.find "byr" m) |> hasLength 4
        && (Map.find "iyr" m) |> hasLength 4
        && (Map.find "eyr" m) |> hasLength 4
        && (Map.find "ecl" m) |> hasLength 3
        && (Map.find "hcl" m) |> hasLength 7
        && (Map.find "pid" m) |> hasLength 9
        && (Map.find "hcl" m) |> isLowercase
        // There is something I'm missing here,,, not sure what...

let solve (rows : string list) =
    let parsed = rows |> List.collect runParseRow

    let combined = combineRows Map.empty parsed


    let a = combined |> List.filter isValidPassport |> List.length
    let b = combined |> List.filter isValidPassport |> List.filter isReallyValid |> List.length

    printfn "A=%i" a
    printfn "B=%i" b
    0
