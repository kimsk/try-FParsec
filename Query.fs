module Qry.Query

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
