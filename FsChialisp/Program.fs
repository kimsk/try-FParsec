open Chialisp

open FParsec


let test parser input =
    let result: ParserResult<_, unit> = run parser input
    let padInput = input.PadRight 30

    match result with
    | Success(r, _, _) -> printfn "%s-> %O" padInput r
    | Failure(e, _, _) -> printfn "%O" e

test expr "'Hello'"
