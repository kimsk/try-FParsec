open FParsec
open Json

let test parser input =
    let result: ParserResult<_, unit> = run parser input
    let padInput = input.PadRight 30

    match result with
    | Success(r, _, _) -> printfn "%s-> %O" padInput r
    | Failure(e, _, _) -> printfn "%O" e

test jnull "null"
test jtrue "true"
test jfalse "false"
test jbool "true"
test jnumber "3.14"

//test (anyOf "\"\\/bfnrt") "b"
//test stringLiteral "\"Hello, World!\""
test jstring "\"Hello, World!\""

test keyValue "\"key\":\"value\""
test keyValue "\"key\":true"
test keyValue "\"key\":2"

test jlist "[1, true, 3, \"Hello, World!\", {\"key\": 42}, {\"a\": {\"aa\": true}}]"

let testJson =
    """
[
    1, true,
    {
        "a": "Hello",
        "b": "World",
        "c": {
            "aa": true
        }
    }
]
"""

test json testJson
