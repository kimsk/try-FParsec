module Qry.Query

open FParsec

type BinaryExprKind =
    // mathematical
    | Add
    | Subtract
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

let ws = skipMany (skipChar ' ')
let ws1 = skipMany1 (skipChar ' ')

let quote: Parser<_, unit> = skipChar '\''

let intOrFloatLiteral =
    numberLiteral (NumberLiteralOptions.DefaultFloat ||| NumberLiteralOptions.DefaultInteger) "number"
    |>> fun n ->
            if n.IsInteger then
                Expr.IntLiteral(int n.String)
            else
                Expr.FloatLiteral(float n.String)
    .>> ws

let stringLiteral =
    quote >>. manyCharsTill anyChar quote |>> Expr.StringLiteral .>> ws

let identifier = many1Chars (letter <|> digit) |>> Expr.Identifier .>> ws

let opp = OperatorPrecedenceParser<Expr, _, _>()

// order is important
opp.TermParser <- choice [ intOrFloatLiteral; stringLiteral; identifier ]

opp.AddOperator
<| InfixOperator("*", ws, 1, Associativity.Left, (fun x y -> Expr.Binary(x, y, BinaryExprKind.Multiply)))

opp.AddOperator
<| InfixOperator("/", ws, 2, Associativity.Left, (fun x y -> Expr.Binary(x, y, BinaryExprKind.Divide)))

opp.AddOperator
<| InfixOperator("-", ws, 3, Associativity.Left, (fun x y -> Expr.Binary(x, y, BinaryExprKind.Subtract)))

opp.AddOperator
<| InfixOperator("+", ws, 4, Associativity.Left, (fun x y -> Expr.Binary(x, y, BinaryExprKind.Add)))

opp.AddOperator
<| InfixOperator("&&", ws, 5, Associativity.Left, (fun x y -> Expr.Binary(x, y, BinaryExprKind.And)))

opp.AddOperator
<| InfixOperator("||", ws, 6, Associativity.Left, (fun x y -> Expr.Binary(x, y, BinaryExprKind.Or)))

opp.AddOperator
<| InfixOperator("=", ws, 7, Associativity.None, (fun x y -> Expr.Binary(x, y, BinaryExprKind.Equals)))

opp.AddOperator
<| InfixOperator("!=", ws, 8, Associativity.None, (fun x y -> Expr.Binary(x, y, BinaryExprKind.NotEquals)))

opp.AddOperator
<| InfixOperator(">", ws, 9, Associativity.None, (fun x y -> Expr.Binary(x, y, BinaryExprKind.GreaterThan)))

opp.AddOperator
<| InfixOperator(">=", ws, 10, Associativity.None, (fun x y -> Expr.Binary(x, y, BinaryExprKind.GreaterThanOrEquals)))

opp.AddOperator
<| InfixOperator("<", ws, 11, Associativity.None, (fun x y -> Expr.Binary(x, y, BinaryExprKind.LesserThan)))

opp.AddOperator
<| InfixOperator("<=", ws, 12, Associativity.None, (fun x y -> Expr.Binary(x, y, BinaryExprKind.LesserThanOrEquals)))


let expr = opp.ExpressionParser

let orderDirAsc = skipString "asc" >>% OrderDir.Ascending .>> ws
let orderDirDesc = skipString "desc" >>% OrderDir.Descending .>> ws
let orderDir = orderDirAsc <|> orderDirDesc

let filterBy = skipString "filterby" >>. ws1 >>. expr .>> ws |>> Stmt.FilterBy

let orderBy =
    skipString "orderby" >>. ws1 >>. expr .>>. orderDir .>> ws |>> Stmt.OrderBy

let skip = skipString "skip" >>. ws1 >>. pint32 .>> ws |>> Stmt.Skip
let take = skipString "take" >>. ws1 >>. pint32 .>> ws |>> Stmt.Take

let stmt = choice [ filterBy; orderBy; skip; take ]

let query = sepEndBy stmt skipNewline |>> fun s -> { Statements = s }

let queryFull = spaces >>. query .>> spaces .>> eof

let parse input =
    match run queryFull input with
    | Success(res, _, _) -> Result.Ok res
    | Failure(err, _, _) -> Result.Error err
