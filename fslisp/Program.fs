open FParsec

open Parser

let test parser input =
    let result: ParserResult<_, unit> = run parser input
    let padInput = input.PadRight 30

    match result with
    | Success(r, _, _) -> printfn "%s-> %O" padInput r
    | Failure(e, _, _) -> printfn "%O" e

    printfn ""

test symbol "abc"
test number "123"
test stringLiteral "\"TEST\""
test list "(() a b 1 2 \"Hello\")"
test list "((a b))"

test code "((q 10) (list 1 2 3) a b 1 2 \"Hello\")"

test code "()"

test code "abc"

test
    list
    """( 1
    )
"""

test
    code
    """
    (
        (

        )
        ()
        1
    )

"""

test
    list
    """(
    (q 10)
    (list 1 2 3)
    a b
    1 2
    "Hello")"""

test
    code
    """
(
    ; This is a comment
    (q 10) (list 1 2 3) a b 1 2 "Hello")
"""
