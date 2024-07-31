module TypesafeUnitTests.Interpolation exposing (..)

import Mustache exposing (htmlEscape, lookup, interpolate, section, invertedSection)

import Test exposing (Test, test)
import Expect exposing (equal)

comment : String
comment = ""

setDelimiter : String -> String -> String
setDelimiter _ _ = ""



{- No Interpolation
The template is:

---
Hello from {Mustache}!

---
The data is:

---
{}
---
-}

render0 : Context0 a -> String
render0 c =
    """Hello from {Mustache}!
"""

type alias Context0 a = { dummy : a }


suite0 : Test
suite0 = test """No Interpolation — Mustache-free templates should render as-is.""" <|
    \_ ->
    { dummy = () }
    |> render0
    |> equal """Hello from {Mustache}!
"""



{- Basic Interpolation
The template is:

---
Hello, {{subject}}!

---
The data is:

---
{
    "subject": "world"
}
---
-}

render1 : Context1 a -> String
render1 c =
    """Hello, """ ++ htmlEscape c.subject ++ """!
"""

type alias Context1 a =
    { a
        | subject : String
    }

suite1 : Test
suite1 = test """Basic Interpolation — Unadorned tags should interpolate content into the template.""" <|
    \_ ->
    { subject =
        """world"""
    }
    |> render1
    |> equal """Hello, world!
"""



{- HTML Escaping
The template is:

---
These characters should be HTML escaped: {{forbidden}}

---
The data is:

---
{
    "forbidden": "& \" < >"
}
---
-}

render2 : Context2 a -> String
render2 c =
    """These characters should be HTML escaped: """ ++ htmlEscape c.forbidden ++ """
"""

type alias Context2 a =
    { a
        | forbidden : String
    }

suite2 : Test
suite2 = test """HTML Escaping — Basic interpolation should be HTML escaped.""" <|
    \_ ->
    { forbidden =
        """& \" < >"""
    }
    |> render2
    |> equal """These characters should be HTML escaped: &amp; &quot; &lt; &gt;
"""



{- Triple Mustache
The template is:

---
These characters should not be HTML escaped: {{{forbidden}}}

---
The data is:

---
{
    "forbidden": "& \" < >"
}
---
-}

render3 : Context3 a -> String
render3 c =
    """These characters should not be HTML escaped: """ ++ c.forbidden ++ """
"""

type alias Context3 a =
    { a
        | forbidden : String
    }

suite3 : Test
suite3 = test """Triple Mustache — Triple mustaches should interpolate without HTML escaping.""" <|
    \_ ->
    { forbidden =
        """& \" < >"""
    }
    |> render3
    |> equal """These characters should not be HTML escaped: & \" < >
"""



{- Ampersand
The template is:

---
These characters should not be HTML escaped: {{&forbidden}}

---
The data is:

---
{
    "forbidden": "& \" < >"
}
---
-}

render4 : Context4 a -> String
render4 c =
    """These characters should not be HTML escaped: """ ++ c.forbidden ++ """
"""

type alias Context4 a =
    { a
        | forbidden : String
    }

suite4 : Test
suite4 = test """Ampersand — Ampersand should interpolate without HTML escaping.""" <|
    \_ ->
    { forbidden =
        """& \" < >"""
    }
    |> render4
    |> equal """These characters should not be HTML escaped: & \" < >
"""



{- Basic Integer Interpolation
The template is:

---
"{{mph}} miles an hour!"
---
The data is:

---
{
    "mph": 85
}
---
-}

render5 : Context5 a -> String
render5 c =
    """\"""" ++ htmlEscape c.mph ++ """ miles an hour!\""""

type alias Context5 a =
    { a
        | mph : String
    }

suite5 : Test
suite5 = test """Basic Integer Interpolation — Integers should interpolate seamlessly.""" <|
    \_ ->
    { mph =
        String.fromInt 85
    }
    |> render5
    |> equal """\"85 miles an hour!\""""



{- Triple Mustache Integer Interpolation
The template is:

---
"{{{mph}}} miles an hour!"
---
The data is:

---
{
    "mph": 85
}
---
-}

render6 : Context6 a -> String
render6 c =
    """\"""" ++ c.mph ++ """ miles an hour!\""""

type alias Context6 a =
    { a
        | mph : String
    }

suite6 : Test
suite6 = test """Triple Mustache Integer Interpolation — Integers should interpolate seamlessly.""" <|
    \_ ->
    { mph =
        String.fromInt 85
    }
    |> render6
    |> equal """\"85 miles an hour!\""""



{- Ampersand Integer Interpolation
The template is:

---
"{{&mph}} miles an hour!"
---
The data is:

---
{
    "mph": 85
}
---
-}

render7 : Context7 a -> String
render7 c =
    """\"""" ++ c.mph ++ """ miles an hour!\""""

type alias Context7 a =
    { a
        | mph : String
    }

suite7 : Test
suite7 = test """Ampersand Integer Interpolation — Integers should interpolate seamlessly.""" <|
    \_ ->
    { mph =
        String.fromInt 85
    }
    |> render7
    |> equal """\"85 miles an hour!\""""



{- Basic Decimal Interpolation
The template is:

---
"{{power}} jiggawatts!"
---
The data is:

---
{
    "power": 1.21
}
---
-}

render8 : Context8 a -> String
render8 c =
    """\"""" ++ htmlEscape c.power ++ """ jiggawatts!\""""

type alias Context8 a =
    { a
        | power : String
    }

suite8 : Test
suite8 = test """Basic Decimal Interpolation — Decimals should interpolate seamlessly with proper significance.""" <|
    \_ ->
    { power =
        String.fromFloat 1.21
    }
    |> render8
    |> equal """\"1.21 jiggawatts!\""""



{- Triple Mustache Decimal Interpolation
The template is:

---
"{{{power}}} jiggawatts!"
---
The data is:

---
{
    "power": 1.21
}
---
-}

render9 : Context9 a -> String
render9 c =
    """\"""" ++ c.power ++ """ jiggawatts!\""""

type alias Context9 a =
    { a
        | power : String
    }

suite9 : Test
suite9 = test """Triple Mustache Decimal Interpolation — Decimals should interpolate seamlessly with proper significance.""" <|
    \_ ->
    { power =
        String.fromFloat 1.21
    }
    |> render9
    |> equal """\"1.21 jiggawatts!\""""



{- Ampersand Decimal Interpolation
The template is:

---
"{{&power}} jiggawatts!"
---
The data is:

---
{
    "power": 1.21
}
---
-}

render10 : Context10 a -> String
render10 c =
    """\"""" ++ c.power ++ """ jiggawatts!\""""

type alias Context10 a =
    { a
        | power : String
    }

suite10 : Test
suite10 = test """Ampersand Decimal Interpolation — Decimals should interpolate seamlessly with proper significance.""" <|
    \_ ->
    { power =
        String.fromFloat 1.21
    }
    |> render10
    |> equal """\"1.21 jiggawatts!\""""



{- Implicit Iterators - Basic Interpolation
The template is:

---
Hello, {{.}}!

---
The data is:

---
"world"
---
-}

render11 : Context11 a -> String
render11 c =
    """Hello, """ ++ htmlEscape c.implicit ++ """!
"""

type alias Context11 a =
    { a
        | implicit : String
    }

suite11 : Test
suite11 = test """Implicit Iterators - Basic Interpolation — Unadorned tags should interpolate content into the template.""" <|
    \_ ->
    { implicit = """world""" }
    |> render11
    |> equal """Hello, world!
"""



{- Implicit Iterators - HTML Escaping
The template is:

---
These characters should be HTML escaped: {{.}}

---
The data is:

---
"& \" < >"
---
-}

render12 : Context12 a -> String
render12 c =
    """These characters should be HTML escaped: """ ++ htmlEscape c.implicit ++ """
"""

type alias Context12 a =
    { a
        | implicit : String
    }

suite12 : Test
suite12 = test """Implicit Iterators - HTML Escaping — Basic interpolation should be HTML escaped.""" <|
    \_ ->
    { implicit = """& \" < >""" }
    |> render12
    |> equal """These characters should be HTML escaped: &amp; &quot; &lt; &gt;
"""



{- Implicit Iterators - Triple Mustache
The template is:

---
These characters should not be HTML escaped: {{{.}}}

---
The data is:

---
"& \" < >"
---
-}

render13 : Context13 a -> String
render13 c =
    """These characters should not be HTML escaped: """ ++ c.implicit ++ """
"""

type alias Context13 a =
    { a
        | implicit : String
    }

suite13 : Test
suite13 = test """Implicit Iterators - Triple Mustache — Triple mustaches should interpolate without HTML escaping.""" <|
    \_ ->
    { implicit = """& \" < >""" }
    |> render13
    |> equal """These characters should not be HTML escaped: & \" < >
"""



{- Implicit Iterators - Ampersand
The template is:

---
These characters should not be HTML escaped: {{&.}}

---
The data is:

---
"& \" < >"
---
-}

render14 : Context14 a -> String
render14 c =
    """These characters should not be HTML escaped: """ ++ c.implicit ++ """
"""

type alias Context14 a =
    { a
        | implicit : String
    }

suite14 : Test
suite14 = test """Implicit Iterators - Ampersand — Ampersand should interpolate without HTML escaping.""" <|
    \_ ->
    { implicit = """& \" < >""" }
    |> render14
    |> equal """These characters should not be HTML escaped: & \" < >
"""



{- Implicit Iterators - Basic Integer Interpolation
The template is:

---
"{{.}} miles an hour!"
---
The data is:

---
85
---
-}

render15 : Context15 a -> String
render15 c =
    """\"""" ++ htmlEscape c.implicit ++ """ miles an hour!\""""

type alias Context15 a =
    { a
        | implicit : String
    }

suite15 : Test
suite15 = test """Implicit Iterators - Basic Integer Interpolation — Integers should interpolate seamlessly.""" <|
    \_ ->
    { implicit = String.fromInt 85 }
    |> render15
    |> equal """\"85 miles an hour!\""""



{- Interpolation - Surrounding Whitespace
The template is:

---
| {{string}} |
---
The data is:

---
{
    "string": "---"
}
---
-}

render16 : Context16 a -> String
render16 c =
    """| """ ++ htmlEscape c.string ++ """ |"""

type alias Context16 a =
    { a
        | string : String
    }

suite16 : Test
suite16 = test """Interpolation - Surrounding Whitespace — Interpolation should not alter surrounding whitespace.""" <|
    \_ ->
    { string =
        """---"""
    }
    |> render16
    |> equal """| --- |"""



{- Triple Mustache - Surrounding Whitespace
The template is:

---
| {{{string}}} |
---
The data is:

---
{
    "string": "---"
}
---
-}

render17 : Context17 a -> String
render17 c =
    """| """ ++ c.string ++ """ |"""

type alias Context17 a =
    { a
        | string : String
    }

suite17 : Test
suite17 = test """Triple Mustache - Surrounding Whitespace — Interpolation should not alter surrounding whitespace.""" <|
    \_ ->
    { string =
        """---"""
    }
    |> render17
    |> equal """| --- |"""



{- Ampersand - Surrounding Whitespace
The template is:

---
| {{&string}} |
---
The data is:

---
{
    "string": "---"
}
---
-}

render18 : Context18 a -> String
render18 c =
    """| """ ++ c.string ++ """ |"""

type alias Context18 a =
    { a
        | string : String
    }

suite18 : Test
suite18 = test """Ampersand - Surrounding Whitespace — Interpolation should not alter surrounding whitespace.""" <|
    \_ ->
    { string =
        """---"""
    }
    |> render18
    |> equal """| --- |"""



{- Interpolation - Standalone
The template is:

---
  {{string}}

---
The data is:

---
{
    "string": "---"
}
---
-}

render19 : Context19 a -> String
render19 c =
    """  """ ++ htmlEscape c.string ++ """
"""

type alias Context19 a =
    { a
        | string : String
    }

suite19 : Test
suite19 = test """Interpolation - Standalone — Standalone interpolation should not alter surrounding whitespace.""" <|
    \_ ->
    { string =
        """---"""
    }
    |> render19
    |> equal """  ---
"""



{- Triple Mustache - Standalone
The template is:

---
  {{{string}}}

---
The data is:

---
{
    "string": "---"
}
---
-}

render20 : Context20 a -> String
render20 c =
    """  """ ++ c.string ++ """
"""

type alias Context20 a =
    { a
        | string : String
    }

suite20 : Test
suite20 = test """Triple Mustache - Standalone — Standalone interpolation should not alter surrounding whitespace.""" <|
    \_ ->
    { string =
        """---"""
    }
    |> render20
    |> equal """  ---
"""



{- Ampersand - Standalone
The template is:

---
  {{&string}}

---
The data is:

---
{
    "string": "---"
}
---
-}

render21 : Context21 a -> String
render21 c =
    """  """ ++ c.string ++ """
"""

type alias Context21 a =
    { a
        | string : String
    }

suite21 : Test
suite21 = test """Ampersand - Standalone — Standalone interpolation should not alter surrounding whitespace.""" <|
    \_ ->
    { string =
        """---"""
    }
    |> render21
    |> equal """  ---
"""



{- Interpolation With Padding
The template is:

---
|{{ string }}|
---
The data is:

---
{
    "string": "---"
}
---
-}

render22 : Context22 a -> String
render22 c =
    """|""" ++ htmlEscape c.string ++ """|"""

type alias Context22 a =
    { a
        | string : String
    }

suite22 : Test
suite22 = test """Interpolation With Padding — Superfluous in-tag whitespace should be ignored.""" <|
    \_ ->
    { string =
        """---"""
    }
    |> render22
    |> equal """|---|"""



{- Triple Mustache With Padding
The template is:

---
|{{{ string }}}|
---
The data is:

---
{
    "string": "---"
}
---
-}

render23 : Context23 a -> String
render23 c =
    """|""" ++ c.string ++ """|"""

type alias Context23 a =
    { a
        | string : String
    }

suite23 : Test
suite23 = test """Triple Mustache With Padding — Superfluous in-tag whitespace should be ignored.""" <|
    \_ ->
    { string =
        """---"""
    }
    |> render23
    |> equal """|---|"""



{- Ampersand With Padding
The template is:

---
|{{& string }}|
---
The data is:

---
{
    "string": "---"
}
---
-}

render24 : Context24 a -> String
render24 c =
    """|""" ++ c.string ++ """|"""

type alias Context24 a =
    { a
        | string : String
    }

suite24 : Test
suite24 = test """Ampersand With Padding — Superfluous in-tag whitespace should be ignored.""" <|
    \_ ->
    { string =
        """---"""
    }
    |> render24
    |> equal """|---|"""

