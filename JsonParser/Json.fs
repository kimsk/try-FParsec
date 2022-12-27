module Json

open FParsec

type Json =
    | JNull
    | JBool of bool
    | JNumber of float
    | JString of string
    | JList of Json list
    | JObject of Map<string, Json>

let jnull: Parser<Json, unit> = stringReturn "null" JNull
let jtrue: Parser<Json, unit> = stringReturn "true" (JBool true)
let jfalse: Parser<Json, unit> = stringReturn "false" (JBool false)
let jbool: Parser<Json, unit> = jtrue <|> jfalse
let jnumber: Parser<Json, unit> = pfloat |>> JNumber

// \b backspace
// \f form feed
// \n new line
// \r carriage return
// \t tab

let str s = pstring s

let stringLiteral: Parser<string, unit> =
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

let jstring = stringLiteral |>> JString

let jvalue, jvalueRef = createParserForwardedToRef<Json, unit> ()

let listBetweenStrings sOpen sClose pElement f =
    between (str sOpen) (str sClose) (spaces >>. sepBy (pElement .>> spaces) (str "," >>. spaces) |>> f)

let jlist = listBetweenStrings "[" "]" jvalue JList

let keyValue = stringLiteral .>>. (spaces >>. str ":" >>. spaces >>. jvalue)
let jobject = listBetweenStrings "{" "}" keyValue (Map.ofList >> JObject)


do jvalueRef := choice [ jnull; jbool; jnumber; jstring; jlist; jobject ]

let json = spaces >>. jvalue .>> spaces .>> eof
