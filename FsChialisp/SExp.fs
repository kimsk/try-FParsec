module Chialisp

open FParsec

type SExp =
    | Nil
    | Cons of (SExp * SExp)
    | Integer of int
    | QuotedString of (char * string)
    | Atom of string


let singleQuote: Parser<char, unit> = pchar '''
let doubleQuote: Parser<char, unit> = pchar '"'
let openParenthesis: Parser<_, unit> = pchar '(' .>> spaces
let closeParenthesis: Parser<_, unit> = spaces .>> pchar ')'
let dot: Parser<_, unit> = pchar '.' .>> spaces


//let nil = openParenthesis >>. spaces >>. closeParenthesis |>> SExp.Nil .>> spaces


let atom =
    singleQuote >>. manyCharsTill anyChar singleQuote |>> SExp.Atom .>> spaces

//let opp: Prefix<Expr, unit, unit> = OperatorPrecedenceParser<Expr, _, _>()

let expr = atom
