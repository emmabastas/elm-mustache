module TypesafeUnitTests.Delimiters exposing (..)

import Mustache exposing (htmlEscape, lookup, interpolate, section, invertedSection)

import Test exposing (Test, test)
import Expect exposing (equal)

comment : String
comment = ""

setDelimiter : String -> String -> String
setDelimiter _ _ = ""



{- Pair Behavior
The template is:

---
{{=<% %>=}}(<%text%>)
---
The data is:

---
{
    "text": "Hey!"
}
---
-}

render0 : Context0 a -> String
render0 c =
    (setDelimiter "<%" "%>") ++ """(""" ++ htmlEscape c.text ++ """)"""

type alias Context0 a =
    { a
        | text : String
    }

suite0 : Test
suite0 = test """Pair Behavior — The equals sign (used on both sides) should permit delimiter changes.""" <|
    \_ ->
    { text =
        """Hey!"""
    }
    |> render0
    |> equal """(Hey!)"""



{- Special Characters
The template is:

---
({{=[ ]=}}[text])
---
The data is:

---
{
    "text": "It worked!"
}
---
-}

render1 : Context1 a -> String
render1 c =
    """(""" ++ (setDelimiter "[" "]") ++ htmlEscape c.text ++ """)"""

type alias Context1 a =
    { a
        | text : String
    }

suite1 : Test
suite1 = test """Special Characters — Characters with special meaning regexen should be valid delimiters.""" <|
    \_ ->
    { text =
        """It worked!"""
    }
    |> render1
    |> equal """(It worked!)"""



{- Sections
The template is:

---
[
{{#section}}
  {{data}}
  |data|
{{/section}}

{{= | | =}}
|#section|
  {{data}}
  |data|
|/section|
]

---
The data is:

---
{
    "section": true,
    "data": "I got interpolated."
}
---
-}

render2 : Context2 a -> String
render2 c =
    """[
""" ++ (if c.section then ("""  """ ++ htmlEscape c.data ++ """
  |data|
""") else "") ++ """
""" ++ (setDelimiter "|" "|") ++ (if c.section then ("""  {{data}}
  """ ++ htmlEscape c.data ++ """
""") else "") ++ """]
"""

type alias Context2 a =
    { a
        | section : Bool
        , data : String
    }

suite2 : Test
suite2 = test """Sections — Delimiters set outside sections should persist.""" <|
    \_ ->
    { section =
        True
    , data =
        """I got interpolated."""
    }
    |> render2
    |> equal """[
  I got interpolated.
  |data|

  {{data}}
  I got interpolated.
]
"""



{- Inverted Sections
The template is:

---
[
{{^section}}
  {{data}}
  |data|
{{/section}}

{{= | | =}}
|^section|
  {{data}}
  |data|
|/section|
]

---
The data is:

---
{
    "section": false,
    "data": "I got interpolated."
}
---
-}

render3 : Context3 a -> String
render3 c =
    """[
""" ++ (if not c.section then ("""  """ ++ htmlEscape c.data ++ """
  |data|
""") else "") ++ """
""" ++ (setDelimiter "|" "|") ++ (if not c.section then ("""  {{data}}
  """ ++ htmlEscape c.data ++ """
""") else "") ++ """]
"""

type alias Context3 a =
    { a
        | section : Bool
        , data : String
    }

suite3 : Test
suite3 = test """Inverted Sections — Delimiters set outside inverted sections should persist.""" <|
    \_ ->
    { section =
        False
    , data =
        """I got interpolated."""
    }
    |> render3
    |> equal """[
  I got interpolated.
  |data|

  {{data}}
  I got interpolated.
]
"""



{- Surrounding Whitespace
The template is:

---
| {{=@ @=}} |
---
The data is:

---
{}
---
-}

render4 : Context4 a -> String
render4 c =
    """| """ ++ (setDelimiter "@" "@") ++ """ |"""

type alias Context4 a = { dummy : a }


suite4 : Test
suite4 = test """Surrounding Whitespace — Surrounding whitespace should be left untouched.""" <|
    \_ ->
    { dummy = () }
    |> render4
    |> equal """|  |"""



{- Outlying Whitespace (Inline)
The template is:

---
 | {{=@ @=}}

---
The data is:

---
{}
---
-}

render5 : Context5 a -> String
render5 c =
    """ | """ ++ (setDelimiter "@" "@") ++ """
"""

type alias Context5 a = { dummy : a }


suite5 : Test
suite5 = test """Outlying Whitespace (Inline) — Whitespace should be left untouched.""" <|
    \_ ->
    { dummy = () }
    |> render5
    |> equal """ | 
"""



{- Standalone Tag
The template is:

---
Begin.
{{=@ @=}}
End.

---
The data is:

---
{}
---
-}

render6 : Context6 a -> String
render6 c =
    """Begin.
""" ++ (setDelimiter "@" "@") ++ """End.
"""

type alias Context6 a = { dummy : a }


suite6 : Test
suite6 = test """Standalone Tag — Standalone lines should be removed from the template.""" <|
    \_ ->
    { dummy = () }
    |> render6
    |> equal """Begin.
End.
"""



{- Indented Standalone Tag
The template is:

---
Begin.
  {{=@ @=}}
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
""" ++ (setDelimiter "@" "@") ++ """End.
"""

type alias Context7 a = { dummy : a }


suite7 : Test
suite7 = test """Indented Standalone Tag — Indented standalone lines should be removed from the template.""" <|
    \_ ->
    { dummy = () }
    |> render7
    |> equal """Begin.
End.
"""



{- Standalone Line Endings
The template is:

---
|
{{= @ @ =}}
|
---
The data is:

---
{}
---
-}

render8 : Context8 a -> String
render8 c =
    """|
""" ++ (setDelimiter "@" "@") ++ """|"""

type alias Context8 a = { dummy : a }


suite8 : Test
suite8 = test """Standalone Line Endings — "\r\n" should be considered a newline for standalone tags.""" <|
    \_ ->
    { dummy = () }
    |> render8
    |> equal """|
|"""



{- Standalone Without Previous Line
The template is:

---
  {{=@ @=}}
=
---
The data is:

---
{}
---
-}

render9 : Context9 a -> String
render9 c =
    (setDelimiter "@" "@") ++ """="""

type alias Context9 a = { dummy : a }


suite9 : Test
suite9 = test """Standalone Without Previous Line — Standalone tags should not require a newline to precede them.""" <|
    \_ ->
    { dummy = () }
    |> render9
    |> equal """="""



{- Standalone Without Newline
The template is:

---
=
  {{=@ @=}}
---
The data is:

---
{}
---
-}

render10 : Context10 a -> String
render10 c =
    """=
""" ++ (setDelimiter "@" "@")

type alias Context10 a = { dummy : a }


suite10 : Test
suite10 = test """Standalone Without Newline — Standalone tags should not require a newline to follow them.""" <|
    \_ ->
    { dummy = () }
    |> render10
    |> equal """=
"""



{- Pair with Padding
The template is:

---
|{{= @   @ =}}|
---
The data is:

---
{}
---
-}

render11 : Context11 a -> String
render11 c =
    """|""" ++ (setDelimiter "@" "") ++ """|"""

type alias Context11 a = { dummy : a }


suite11 : Test
suite11 = test """Pair with Padding — Superfluous in-tag whitespace should be ignored.""" <|
    \_ ->
    { dummy = () }
    |> render11
    |> equal """||"""

