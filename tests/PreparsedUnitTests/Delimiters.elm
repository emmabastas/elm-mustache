module PreparsedUnitTests.Delimiters exposing (..)

import Json.Decode as D exposing (Value)
import Json.Encode as E
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

render0 : Value -> String
render0 json = (\context0 ->
    (setDelimiter "<%" "%>") ++ """(""" ++ htmlEscape (interpolate (lookup context0 ["text"])) ++ """)""") [json]

suite0 : Test
suite0 = test """Pair Behavior — The equals sign (used on both sides) should permit delimiter changes.""" <|
    \_ ->
    (E.object
      [ ( "text"
        , (E.string """Hey!""")
        )
      ])
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

render1 : Value -> String
render1 json = (\context0 ->
    """(""" ++ (setDelimiter "[" "]") ++ htmlEscape (interpolate (lookup context0 ["text"])) ++ """)""") [json]

suite1 : Test
suite1 = test """Special Characters — Characters with special meaning regexen should be valid delimiters.""" <|
    \_ ->
    (E.object
      [ ( "text"
        , (E.string """It worked!""")
        )
      ])
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

render2 : Value -> String
render2 json = (\context0 ->
    """[
""" ++ (section context0 ["section"] (\context1 ->"""  """ ++ htmlEscape (interpolate (lookup context1 ["data"])) ++ """
  |data|
""")) ++ """
""" ++ (setDelimiter "|" "|") ++ (section context0 ["section"] (\context1 ->"""  {{data}}
  """ ++ htmlEscape (interpolate (lookup context1 ["data"])) ++ """
""")) ++ """]
""") [json]

suite2 : Test
suite2 = test """Sections — Delimiters set outside sections should persist.""" <|
    \_ ->
    (E.object
      [ ( "section"
        , (E.bool True)
        )
      , ( "data"
        , (E.string """I got interpolated.""")
        )
      ])
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

render3 : Value -> String
render3 json = (\context0 ->
    """[
""" ++ (invertedSection context0 ["section"] (\context1 ->"""  """ ++ htmlEscape (interpolate (lookup context1 ["data"])) ++ """
  |data|
""")) ++ """
""" ++ (setDelimiter "|" "|") ++ (invertedSection context0 ["section"] (\context1 ->"""  {{data}}
  """ ++ htmlEscape (interpolate (lookup context1 ["data"])) ++ """
""")) ++ """]
""") [json]

suite3 : Test
suite3 = test """Inverted Sections — Delimiters set outside inverted sections should persist.""" <|
    \_ ->
    (E.object
      [ ( "section"
        , (E.bool False)
        )
      , ( "data"
        , (E.string """I got interpolated.""")
        )
      ])
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

render4 : Value -> String
render4 json = (\context0 ->
    """| """ ++ (setDelimiter "@" "@") ++ """ |""") [json]

suite4 : Test
suite4 = test """Surrounding Whitespace — Surrounding whitespace should be left untouched.""" <|
    \_ ->
    (E.object [])
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

render5 : Value -> String
render5 json = (\context0 ->
    """ | """ ++ (setDelimiter "@" "@") ++ """
""") [json]

suite5 : Test
suite5 = test """Outlying Whitespace (Inline) — Whitespace should be left untouched.""" <|
    \_ ->
    (E.object [])
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

render6 : Value -> String
render6 json = (\context0 ->
    """Begin.
""" ++ (setDelimiter "@" "@") ++ """End.
""") [json]

suite6 : Test
suite6 = test """Standalone Tag — Standalone lines should be removed from the template.""" <|
    \_ ->
    (E.object [])
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

render7 : Value -> String
render7 json = (\context0 ->
    """Begin.
""" ++ (setDelimiter "@" "@") ++ """End.
""") [json]

suite7 : Test
suite7 = test """Indented Standalone Tag — Indented standalone lines should be removed from the template.""" <|
    \_ ->
    (E.object [])
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

render8 : Value -> String
render8 json = (\context0 ->
    """|
""" ++ (setDelimiter "@" "@") ++ """|""") [json]

suite8 : Test
suite8 = test """Standalone Line Endings — "\r\n" should be considered a newline for standalone tags.""" <|
    \_ ->
    (E.object [])
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

render9 : Value -> String
render9 json = (\context0 ->
    (setDelimiter "@" "@") ++ """=""") [json]

suite9 : Test
suite9 = test """Standalone Without Previous Line — Standalone tags should not require a newline to precede them.""" <|
    \_ ->
    (E.object [])
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

render10 : Value -> String
render10 json = (\context0 ->
    """=
""" ++ (setDelimiter "@" "@")) [json]

suite10 : Test
suite10 = test """Standalone Without Newline — Standalone tags should not require a newline to follow them.""" <|
    \_ ->
    (E.object [])
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

render11 : Value -> String
render11 json = (\context0 ->
    """|""" ++ (setDelimiter "@" "") ++ """|""") [json]

suite11 : Test
suite11 = test """Pair with Padding — Superfluous in-tag whitespace should be ignored.""" <|
    \_ ->
    (E.object [])
    |> render11
    |> equal """||"""

