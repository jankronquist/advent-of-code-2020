module Day7

open System
open FParsec

type Bag = {Style:string; Color:string}
let makeBag s c = {Style=s; Color=c}

let oneSpace = skipChar ' '
let parseWord = manyChars letter
let parseBag = pipe2 (parseWord .>> oneSpace) (parseWord .>> oneSpace .>> parseWord) makeBag
let parseBagCount = tuple2 (pint32 .>> spaces1) parseBag
let ruleSep = skipChar ',' .>> skipChar ' '
let parseRule = tuple2 (parseBag .>> spaces1 .>> parseWord .>> spaces1) (sepBy parseBagCount ruleSep)

let runParse parser s =
    match run parser s with
    | Success(result, _, _)   -> Some result
    | Failure(errorMsg, _, _) -> None


let getParents parents (parentBag, contents) =
    let addToList l = Some (parentBag :: (Option.defaultValue List.empty l))
    let addDirectParents p (c, bag) = Map.change bag addToList p
    List.fold addDirectParents parents contents

let solve (rows : string list) =
    let parsed = List.choose (runParse parseRule) rows
    let directParents = List.fold getParents Map.empty parsed

    let rec findTransitive b =
        let parents = Map.tryFind b directParents |> Option.defaultValue List.empty
        let transitiveParents = List.collect findTransitive parents
        List.append parents transitiveParents

    let shinyGold = {Style="shiny"; Color="gold"}

    let allParents = findTransitive shinyGold

    printfn "A=%i" (Set.ofList allParents |> Set.count)

    let directChildren : Map<Bag, (int32 * Bag) list> = Map.ofList parsed
    let rec findChildBagCount b =
        match Map.tryFind b directChildren with
        | Some (children) ->
            if List.isEmpty children
            then 0
            else
                (List.sumBy (fun (c, child) -> c * (1 + (findChildBagCount child))) children)
        | _ -> 0

    printfn "B=%i" (findChildBagCount shinyGold)
    0
