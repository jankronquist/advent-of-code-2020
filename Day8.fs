module Day8

open System
open FParsec

type ProgramResult =
    | InfiniteLoop of int32
    | Halt of int32

type Command =
    | Acc of int32
    | Jmp of int32
    | Nop of int32

let parseAcc = pstring "acc " >>. pint32 |>> Acc
let parseJmp = pstring "jmp " >>. pint32 |>> Jmp
let parseNop = pstring "nop " >>. pint32 |>> Nop

let parseCommand = parseAcc <|> parseJmp <|> parseNop

let runParse parser s =
    match run parser s with
    | Success(result, _, _)   -> Some result
    | Failure(errorMsg, _, _) -> None

let solve (rows : string list) =
    let parsedCommands = List.choose (runParse parseCommand) rows
    let rec compute commands acc currentLine linesVisisted =
        if Set.contains currentLine linesVisisted
        then InfiniteLoop acc
        elif currentLine >= List.length commands then Halt acc
        else match commands.[currentLine] with
             | Acc x -> compute commands (acc + x) (currentLine + 1) (Set.add currentLine linesVisisted)
             | Jmp x -> compute commands acc (currentLine + x) (Set.add currentLine linesVisisted)
             | Nop x -> compute commands acc (currentLine + 1) (Set.add currentLine linesVisisted)

    printfn "A=%A" (compute parsedCommands 0 0 Set.empty)

    let fixBug = List.indexed parsedCommands |> List.pick (fun (i, cmd) ->
        match cmd with
        | Acc x -> None
        | Nop x -> if i + x >= List.length parsedCommands
                    then Some i // TODO: run compute
                    else None
        | Jmp x ->
                let keepOrReplace j c = if i = j then Nop x else c
                let replacedCommands = List.mapi keepOrReplace parsedCommands
                match (compute replacedCommands 0 0 Set.empty) with
                | InfiniteLoop _ -> None
                | Halt x -> Some x
    )

    printfn "B=%A" fixBug
    0
