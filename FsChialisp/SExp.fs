module Chialisp

open FParsec

type SExp =
    | Nil
    | Atom of string
    | Integer of int
    | QuotedString of (char * string)
    | Cons of (SExp * SExp)


let singleQuote: Parser<_, unit> = skipChar '''
let doubleQuote: Parser<_, unit> = skipChar '"'
let openParenthesis: Parser<_, unit> = skipChar '(' >>. spaces
let closeParenthesis: Parser<_, unit> = spaces .>> skipChar ')'
let dot: Parser<_, unit> = skipChar '.' >>. spaces


let nil = between openParenthesis closeParenthesis spaces >>% SExp.Nil

let atom =
    singleQuote >>. manyCharsTill anyChar singleQuote |>> SExp.Atom .>> spaces

//let opp: Prefix<Expr, unit, unit> = OperatorPrecedenceParser<Expr, _, _>()

let expr = choice [ atom; nil ]
