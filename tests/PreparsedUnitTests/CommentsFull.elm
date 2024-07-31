module PreparsedUnitTests.CommentsFull exposing (..)

import Json.Decode as D exposing (Value)
import Json.Encode as E
import Mustache exposing (htmlEscape, lookup, interpolate, section, invertedSection)

import Test exposing (Test, test)
import Expect exposing (equal)

comment : String
comment = ""

setDelimiter : String -> String -> String
setDelimiter _ _ = ""



{- Inline
The template is:

---
12345{{! Comment Block! }}67890
---
The data is:

---
{}
---
-}

render0 : Value -> String
render0 json = (\context0 ->
    """12345""" ++ comment ++ """67890""") [json]

suite0 : Test
suite0 = test """Inline — Comment blocks should be removed from the template.""" <|
    \_ ->
    (E.object [])
    |> render0
    |> equal """1234567890"""



{- Multiline
The template is:

---
12345{{!
  This is a
  multi-line comment...
}}67890

---
The data is:

---
{}
---
-}

render1 : Value -> String
render1 json = (\context0 ->
    """12345""" ++ comment ++ """67890
""") [json]

suite1 : Test
suite1 = test """Multiline — Multiline comments should be permitted.""" <|
    \_ ->
    (E.object [])
    |> render1
    |> equal """1234567890
"""



{- Standalone
The template is:

---
Begin.
{{! Comment Block! }}
End.

---
The data is:

---
{}
---
-}

render2 : Value -> String
render2 json = (\context0 ->
    """Begin.
""" ++ comment ++ """End.
""") [json]

suite2 : Test
suite2 = test """Standalone — All standalone comment lines should be removed.""" <|
    \_ ->
    (E.object [])
    |> render2
    |> equal """Begin.
End.
"""



{- Indented Standalone
The template is:

---
Begin.
  {{! Indented Comment Block! }}
End.

---
The data is:

---
{}
---
-}

render3 : Value -> String
render3 json = (\context0 ->
    """Begin.
""" ++ comment ++ """End.
""") [json]

suite3 : Test
suite3 = test """Indented Standalone — All standalone comment lines should be removed.""" <|
    \_ ->
    (E.object [])
    |> render3
    |> equal """Begin.
End.
"""



{- Standalone Line Endings
The template is:

---
|
{{! Standalone Comment }}
|
---
The data is:

---
{}
---
-}

render4 : Value -> String
render4 json = (\context0 ->
    """|
""" ++ comment ++ """|""") [json]

suite4 : Test
suite4 = test """Standalone Line Endings — "\r\n" should be considered a newline for standalone tags.""" <|
    \_ ->
    (E.object [])
    |> render4
    |> equal """|
|"""



{- Standalone Without Previous Line
The template is:

---
  {{! I'm Still Standalone }}
!
---
The data is:

---
{}
---
-}

render5 : Value -> String
render5 json = (\context0 ->
    comment ++ """!""") [json]

suite5 : Test
suite5 = test """Standalone Without Previous Line — Standalone tags should not require a newline to precede them.""" <|
    \_ ->
    (E.object [])
    |> render5
    |> equal """!"""



{- Standalone Without Newline
The template is:

---
!
  {{! I'm Still Standalone }}
---
The data is:

---
{}
---
-}

render6 : Value -> String
render6 json = (\context0 ->
    """!
""" ++ comment) [json]

suite6 : Test
suite6 = test """Standalone Without Newline — Standalone tags should not require a newline to follow them.""" <|
    \_ ->
    (E.object [])
    |> render6
    |> equal """!
"""



{- Multiline Standalone
The template is:

---
Begin.
{{!
Something's going on here...
}}
End.

---
The data is:

---
{}
---
-}

render7 : Value -> String
render7 json = (\context0 ->
    """Begin.
""" ++ comment ++ """End.
""") [json]

suite7 : Test
suite7 = test """Multiline Standalone — All standalone comment lines should be removed.""" <|
    \_ ->
    (E.object [])
    |> render7
    |> equal """Begin.
End.
"""



{- Indented Multiline Standalone
The template is:

---
Begin.
  {{!
    Something's going on here...
  }}
End.

---
The data is:

---
{}
---
-}

render8 : Value -> String
render8 json = (\context0 ->
    """Begin.
""" ++ comment ++ """End.
""") [json]

suite8 : Test
suite8 = test """Indented Multiline Standalone — All standalone comment lines should be removed.""" <|
    \_ ->
    (E.object [])
    |> render8
    |> equal """Begin.
End.
"""



{- Indented Inline
The template is:

---
  12 {{! 34 }}

---
The data is:

---
{}
---
-}

render9 : Value -> String
render9 json = (\context0 ->
    """  12 """ ++ comment ++ """
""") [json]

suite9 : Test
suite9 = test """Indented Inline — Inline comments should not strip whitespace""" <|
    \_ ->
    (E.object [])
    |> render9
    |> equal """  12 
"""



{- Surrounding Whitespace
The template is:

---
12345 {{! Comment Block! }} 67890
---
The data is:

---
{}
---
-}

render10 : Value -> String
render10 json = (\context0 ->
    """12345 """ ++ comment ++ """ 67890""") [json]

suite10 : Test
suite10 = test """Surrounding Whitespace — Comment removal should preserve surrounding whitespace.""" <|
    \_ ->
    (E.object [])
    |> render10
    |> equal """12345  67890"""



{- Variable Name Collision
The template is:

---
comments never show: >{{! comment }}<
---
The data is:

---
{
    "! comment": 1,
    "! comment ": 2,
    "!comment": 3,
    "comment": 4
}
---
-}

render11 : Value -> String
render11 json = (\context0 ->
    """comments never show: >""" ++ comment ++ """<""") [json]

suite11 : Test
suite11 = test """Variable Name Collision — Comments must never render, even if variable with same name exists.""" <|
    \_ ->
    (E.object
      [ ( "! comment"
        , (E.int 1)
        )
      , ( "! comment "
        , (E.int 2)
        )
      , ( "!comment"
        , (E.int 3)
        )
      , ( "comment"
        , (E.int 4)
        )
      ])
    |> render11
    |> equal """comments never show: ><"""

