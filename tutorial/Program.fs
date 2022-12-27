open FParsec


let test parser input =
    let result: ParserResult<_, unit> = run parser input
    let padInput = input.PadRight 30

    match result with
    | Success(r, _, _) -> printfn "%s-> %O" padInput r
    | Failure(e, _, _) -> printfn "%O" e

test pfloat "3.14"

let openParenthesis: Parser<_, unit> = pchar '(' .>> spaces
let closeParenthesis: Parser<_, unit> = spaces .>> pchar ')'


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
test floatBetweenParentheses2 "(3.0)"

test (many floatBetweenParentheses2) "(3.14)(1.0)(0.5)(2)"

test (many (floatBetweenBrackets <|> floatBetweenParentheses)) "(3.14)[1.0](0.5)(2)"

let floatList = openParenthesis >>. sepBy pfloat spaces1 .>> closeParenthesis

test floatList "( 3.14 1.0 10  20)"

let a: Parser<_, unit> = pstring "a"

let identifier =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> spaces // skips trailing whitespace

test identifier "_"

let stringLiteral =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')

    let unescape c =
        match c with
        | 'n' -> '\n'
        | 'r' -> '\r'
        | 't' -> '\t'
        | c -> c

    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
    between (pstring "\"") (pstring "\"") (manyChars (normalChar <|> escapedChar))

test stringLiteral "\"abc\""
test stringLiteral "\"abc\\\"def\\\\ghi\""
//test stringLiteral "\"abc\\def\""

let str_ws s = pstring s >>. spaces
let float_ws = pfloat .>> spaces
let product = pipe2 (str_ws "*" >>. float_ws) float_ws (fun x y -> x * y)
test product "* 3 5"

type StringConstant = StringConstant of string * string

let stringConstant =
    pipe3 identifier (str_ws "=") stringLiteral (fun id _ str -> StringConstant(id, str))


test stringConstant "myString = \"stringValue\""

test (float_ws .>>. (str_ws ":" >>. float_ws)) "1:2"

let boolean = (stringReturn "true" true) <|> (stringReturn "false" false)
test boolean "false"
test boolean "true"
