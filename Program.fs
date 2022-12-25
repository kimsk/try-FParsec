open FParsec
open Qry.Query

let result = run stringLiteral "'hello world'"

printf "%O" result
