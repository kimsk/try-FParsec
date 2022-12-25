module Qry.Query

open FParsec

type BinaryExprKind =
    // mathematical
    | Add
    | Substract
    | Multiply
    | Divide
    // logic
    | And
    | Or
    // comparison
    | Equals
    | NotEquals
    | GreaterThan
    | GreaterThanOrEquals
    | LesserThan
    | LesserThanOrEquals

type Expr =
    | IntLiteral of int
    | FloatLiteral of float
    | StringLiteral of string
    | Identifier of string
    | Binary of (Expr * Expr * BinaryExprKind)

type OrderDir =
    | Ascending
    | Descending

type Stmt =
    | FilterBy of Expr
    | OrderBy of Expr * OrderDir
    | Skip of int
    | Take of int

type Query = { Statements: Stmt list }

let a = Expr.StringLiteral "Hello"

let b = Expr.Binary(FloatLiteral 3.14, IntLiteral 42, GreaterThan)

let quote: Parser<_, unit> = skipChar '\''

let intOrFloatLiteral =
    numberLiteral (NumberLiteralOptions.DefaultFloat ||| NumberLiteralOptions.DefaultInteger) "number"
    |>> fun n ->
            if n.IsInteger then
                Expr.IntLiteral(int n.String)
            else
                Expr.FloatLiteral(float n.String)


// combinators
let stringLiteral = quote >>. manyCharsTill anyChar quote |>> Expr.StringLiteral

let identifier = many1Chars (letter <|> digit) |>> Expr.Identifier

// order is important
let expr = choice [ intOrFloatLiteral; stringLiteral; identifier ]
