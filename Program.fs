open System
open Day10

let readInput (day : int) =
    let input = sprintf "./input/day%i.txt" day
    printfn "%s" input
    if System.IO.File.Exists(input)
    then input |> System.IO.File.ReadAllLines |> List.ofArray |> Some
    else None

[<EntryPoint>]
let main argv =
    match readInput 10 with
    | Some rows -> solve rows
    | _ -> 1
