open FParsec
open Qry
open Qry.Query

let books = Books.getAll ()

let parseAndPrint input =
    let result: ParserResult<Expr, unit> = run expr input

    match result with
    | Success(r, _, _) -> printfn "%s -> %O" input r
    | Failure(e, _, _) -> printfn "%O" e


parseAndPrint "'hello world\r\n'"


parseAndPrint "3.14"
parseAndPrint "3.0"
parseAndPrint "3"
parseAndPrint "Category"
parseAndPrint "Category = 'Fantasy'"
//parseAndPrint "= 'Fantasy'"


let input =
    """
filterby Category = 'Fantasy'
orderby Rating desc
skip 0
take 2
"""

let result = parse input

match result with
| Result.Ok res ->
    let queryResult = execute res books
    List.iter (fun i -> printfn "%O" i) queryResult
| Result.Error err -> printfn "%O" err
