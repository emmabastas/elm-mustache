module TypesafeUnitTests.Comments exposing (..)

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

render0 : Context0 a -> String
render0 c =
    """12345""" ++ comment ++ """67890"""

type alias Context0 a = { dummy : a }


suite0 : Test
suite0 = test """Inline — Comment blocks should be removed from the template.""" <|
    \_ ->
    { dummy = () }
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

render1 : Context1 a -> String
render1 c =
    """12345""" ++ comment ++ """67890
"""

type alias Context1 a = { dummy : a }


suite1 : Test
suite1 = test """Multiline — Multiline comments should be permitted.""" <|
    \_ ->
    { dummy = () }
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

render2 : Context2 a -> String
render2 c =
    """Begin.
""" ++ comment ++ """End.
"""

type alias Context2 a = { dummy : a }


suite2 : Test
suite2 = test """Standalone — All standalone comment lines should be removed.""" <|
    \_ ->
    { dummy = () }
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

render3 : Context3 a -> String
render3 c =
    """Begin.
""" ++ comment ++ """End.
"""

type alias Context3 a = { dummy : a }


suite3 : Test
suite3 = test """Indented Standalone — All standalone comment lines should be removed.""" <|
    \_ ->
    { dummy = () }
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

render4 : Context4 a -> String
render4 c =
    """|
""" ++ comment ++ """|"""

type alias Context4 a = { dummy : a }


suite4 : Test
suite4 = test """Standalone Line Endings — "\r\n" should be considered a newline for standalone tags.""" <|
    \_ ->
    { dummy = () }
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

render5 : Context5 a -> String
render5 c =
    comment ++ """!"""

type alias Context5 a = { dummy : a }


suite5 : Test
suite5 = test """Standalone Without Previous Line — Standalone tags should not require a newline to precede them.""" <|
    \_ ->
    { dummy = () }
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

render6 : Context6 a -> String
render6 c =
    """!
""" ++ comment

type alias Context6 a = { dummy : a }


suite6 : Test
suite6 = test """Standalone Without Newline — Standalone tags should not require a newline to follow them.""" <|
    \_ ->
    { dummy = () }
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

render7 : Context7 a -> String
render7 c =
    """Begin.
""" ++ comment ++ """End.
"""

type alias Context7 a = { dummy : a }


suite7 : Test
suite7 = test """Multiline Standalone — All standalone comment lines should be removed.""" <|
    \_ ->
    { dummy = () }
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

render8 : Context8 a -> String
render8 c =
    """Begin.
""" ++ comment ++ """End.
"""

type alias Context8 a = { dummy : a }


suite8 : Test
suite8 = test """Indented Multiline Standalone — All standalone comment lines should be removed.""" <|
    \_ ->
    { dummy = () }
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

render9 : Context9 a -> String
render9 c =
    """  12 """ ++ comment ++ """
"""

type alias Context9 a = { dummy : a }


suite9 : Test
suite9 = test """Indented Inline — Inline comments should not strip whitespace""" <|
    \_ ->
    { dummy = () }
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

render10 : Context10 a -> String
render10 c =
    """12345 """ ++ comment ++ """ 67890"""

type alias Context10 a = { dummy : a }


suite10 : Test
suite10 = test """Surrounding Whitespace — Comment removal should preserve surrounding whitespace.""" <|
    \_ ->
    { dummy = () }
    |> render10
    |> equal """12345  67890"""

