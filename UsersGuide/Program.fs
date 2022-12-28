open FParsec
open Util

let test name parser input =
    let result: ParserResult<_, unit> = run parser input
    let padInput = input.PadRight 30

    match result with
    | Success(r, _, _) -> printfn "%s\r\n%s-> %O" name padInput r
    | Failure(e, _, _) -> printfn "%O" e

test "asciiLower" asciiLower "abc"

test "pint32" pint32 "0xff"

test "skipString \"x\"" (skipString "x") "x"
test "stringReturn \"x\" \"y\"" (stringReturn "x" "y") "x"

let skipString' s = stringReturn s ()
test "stringReturn s ()" (skipString' "x") "x"



let xReturn = stringReturn "x" X
let yReturn = stringReturn "y" (Y 42)
let xReturn' = "xReturn = stringReturn \"x\" X"
let yReturn' = "yReturn = stringReturn \"y\" (Y 42)"
test xReturn' xReturn "xy"
test yReturn' yReturn "yx"

test "(xReturn >>. yReturn)" (xReturn >>. yReturn) "xy"
test "(yReturn .>> xReturn)" (yReturn .>> xReturn) "yx"
test "(yReturn .>>. xReturn)" (yReturn .>>. xReturn) "yx"
