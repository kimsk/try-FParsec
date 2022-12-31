module Parser

open FParsec

type Expr =
    | Comment of string
    | Symbol of string
    | Number of int
    | String of string
    | List of Expr list

let validChar =
    function
    | c when c >= 'a' && c <= 'z' -> true
    | c when c >= 'A' && c <= 'Z' -> true
    | _ -> false

let openParenthesis: Parser<_, unit> = skipChar '(' >>. spaces
let closeParenthesis: Parser<_, unit> = spaces .>> skipChar ')'

let symbol: Parser<Expr, unit> = many1Satisfy validChar |>> Symbol
let number: Parser<Expr, unit> = pint32 |>> Number

let comment: Parser<Expr, unit> =
    spaces >>. skipMany1 (skipChar ';') >>. spaces >>. restOfLine true |>> Comment

let str (s: string) = pstring s

let stringLiteral: Parser<Expr, unit> =
    let escape =
        anyOf "\"\\/bfnrt"
        |>> function
            | 'b' -> "\b"
            | 'f' -> "\u000C"
            | 'n' -> "\n"
            | 'r' -> "\r"
            | 't' -> "\t"
            | c -> string c // every other char is mapped to itself

    let unicodeEscape =
        /// converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
        let hex2int c = (int c &&& 15) + (int c >>> 6) * 9

        str "u"
        >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
            (hex2int h3) * 4096 + (hex2int h2) * 256 + (hex2int h1) * 16 + hex2int h0
            |> char
            |> string)

    let escapedCharSnippet = str "\\" >>. (escape <|> unicodeEscape)
    let normalCharSnippet = manySatisfy (fun c -> c <> '"' && c <> '\\')

    between (str "\"") (str "\"") (stringsSepBy normalCharSnippet escapedCharSnippet)
    |>> String

let expr, exprRef = createParserForwardedToRef<Expr, unit> ()

(* https://stackoverflow.com/questions/52411478/how-to-parse-seq-of-words-separated-by-double-spaces-using-fparsec

This allows us to not report an error for spaces followed by close parenthesis

*)
let listSep = attempt (spaces1 .>> notFollowedBy closeParenthesis)

// let list = (between openParenthesis closeParenthesis (sepBy expr spaces1)) |>> List
let list = (between openParenthesis closeParenthesis (sepBy expr listSep)) |>> List

do exprRef := choice [ list; comment; symbol; number; stringLiteral ]

let code = spaces >>. expr .>> spaces .>> eof
