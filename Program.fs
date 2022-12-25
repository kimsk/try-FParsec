open FParsec
open Qry.Query

let result = run expr "'hello world\r\n'"
printfn "%O" result

let result2 = run expr "3.14"
printfn "%O" result2

let result3 = run expr "3.0"
printfn "%O" result3

let result4 = run expr "3"
printfn "%O" result4

let result5 = run expr "Category"
printfn "%O" result5

let result6 = run expr "Category = 'Fantasy'"
printfn "%O" result6
