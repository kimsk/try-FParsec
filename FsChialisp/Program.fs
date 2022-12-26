open Chialisp

open FParsec


let test parser input =
    let result: ParserResult<_, unit> = run parser input
    let padInput = input.PadRight 20

    match result with
    | Success(r, _, _) -> printfn "%s-> %O" padInput r
    | Failure(e, _, _) -> printfn "%O" e

test expr "'Hello'"

test pfloat "3.14"


let str s = pstring s
let floatBetweenBrackets = str "[" >>. spaces >>. pfloat .>> spaces .>> str "]"
printfn "\nfloatBetweenBrackets"
test floatBetweenBrackets "[ 3.14]"
test floatBetweenBrackets "[3.14]"
test floatBetweenBrackets "[3.14 ]"
test floatBetweenBrackets "[  3.14 ]"

let floatBetweenParentheses = openParenthesis >>. pfloat .>> closeParenthesis
printfn "\nfloatBetweenParentheses"
test floatBetweenParentheses "( 3.14)"
test floatBetweenParentheses "(3.14)"
test floatBetweenParentheses "(3.14 )"
test floatBetweenParentheses "(  3.14 )"

let floatBetweenParentheses2 = between openParenthesis closeParenthesis pfloat
printfn "\nfloatBetweenParentheses2"
test floatBetweenParentheses2 "( 3.14)"
test floatBetweenParentheses2 "(3.14)"
test floatBetweenParentheses2 "(3.14 )"
test floatBetweenParentheses2 "(  3.14 )"
