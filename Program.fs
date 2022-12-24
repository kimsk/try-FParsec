open System
open FParsec

open Qry

// primitives
let p = pstring "hello"

let success = run p "hello"
let failure = run p "bye"

printfn "%O\n%O" success failure

// combinators
let a = pstring "hello"
let b = pstring "world"

let p' = a <|> b
let result = run p' "hello"
printfn "%O" result

type Token =
    | Hello
    | World

let hello_p = pstring "hello" >>% Token.Hello
let world_p = pstring "world" >>% Token.World
let token_p = hello_p <|> world_p

printfn "%O" (run token_p "hello")
printfn "%O" (run token_p "world")
printfn "%O" (run token_p "hell0")
