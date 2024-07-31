module PreparsedUnitTests.Interpolation exposing (..)

import Json.Decode as D exposing (Value)
import Json.Encode as E
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

render0 : Value -> String
render0 json = (\context0 ->
    """Hello from {Mustache}!
""") [json]

suite0 : Test
suite0 = test """No Interpolation — Mustache-free templates should render as-is.""" <|
    \_ ->
    (E.object [])
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

render1 : Value -> String
render1 json = (\context0 ->
    """Hello, """ ++ htmlEscape (interpolate (lookup context0 ["subject"])) ++ """!
""") [json]

suite1 : Test
suite1 = test """Basic Interpolation — Unadorned tags should interpolate content into the template.""" <|
    \_ ->
    (E.object
      [ ( "subject"
        , (E.string """world""")
        )
      ])
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

render2 : Value -> String
render2 json = (\context0 ->
    """These characters should be HTML escaped: """ ++ htmlEscape (interpolate (lookup context0 ["forbidden"])) ++ """
""") [json]

suite2 : Test
suite2 = test """HTML Escaping — Basic interpolation should be HTML escaped.""" <|
    \_ ->
    (E.object
      [ ( "forbidden"
        , (E.string """& \" < >""")
        )
      ])
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

render3 : Value -> String
render3 json = (\context0 ->
    """These characters should not be HTML escaped: """ ++ interpolate (lookup context0 ["forbidden"]) ++ """
""") [json]

suite3 : Test
suite3 = test """Triple Mustache — Triple mustaches should interpolate without HTML escaping.""" <|
    \_ ->
    (E.object
      [ ( "forbidden"
        , (E.string """& \" < >""")
        )
      ])
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

render4 : Value -> String
render4 json = (\context0 ->
    """These characters should not be HTML escaped: """ ++ interpolate (lookup context0 ["forbidden"]) ++ """
""") [json]

suite4 : Test
suite4 = test """Ampersand — Ampersand should interpolate without HTML escaping.""" <|
    \_ ->
    (E.object
      [ ( "forbidden"
        , (E.string """& \" < >""")
        )
      ])
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

render5 : Value -> String
render5 json = (\context0 ->
    """\"""" ++ htmlEscape (interpolate (lookup context0 ["mph"])) ++ """ miles an hour!\"""") [json]

suite5 : Test
suite5 = test """Basic Integer Interpolation — Integers should interpolate seamlessly.""" <|
    \_ ->
    (E.object
      [ ( "mph"
        , (E.int 85)
        )
      ])
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

render6 : Value -> String
render6 json = (\context0 ->
    """\"""" ++ interpolate (lookup context0 ["mph"]) ++ """ miles an hour!\"""") [json]

suite6 : Test
suite6 = test """Triple Mustache Integer Interpolation — Integers should interpolate seamlessly.""" <|
    \_ ->
    (E.object
      [ ( "mph"
        , (E.int 85)
        )
      ])
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

render7 : Value -> String
render7 json = (\context0 ->
    """\"""" ++ interpolate (lookup context0 ["mph"]) ++ """ miles an hour!\"""") [json]

suite7 : Test
suite7 = test """Ampersand Integer Interpolation — Integers should interpolate seamlessly.""" <|
    \_ ->
    (E.object
      [ ( "mph"
        , (E.int 85)
        )
      ])
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

render8 : Value -> String
render8 json = (\context0 ->
    """\"""" ++ htmlEscape (interpolate (lookup context0 ["power"])) ++ """ jiggawatts!\"""") [json]

suite8 : Test
suite8 = test """Basic Decimal Interpolation — Decimals should interpolate seamlessly with proper significance.""" <|
    \_ ->
    (E.object
      [ ( "power"
        , (E.float 1.21)
        )
      ])
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

render9 : Value -> String
render9 json = (\context0 ->
    """\"""" ++ interpolate (lookup context0 ["power"]) ++ """ jiggawatts!\"""") [json]

suite9 : Test
suite9 = test """Triple Mustache Decimal Interpolation — Decimals should interpolate seamlessly with proper significance.""" <|
    \_ ->
    (E.object
      [ ( "power"
        , (E.float 1.21)
        )
      ])
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

render10 : Value -> String
render10 json = (\context0 ->
    """\"""" ++ interpolate (lookup context0 ["power"]) ++ """ jiggawatts!\"""") [json]

suite10 : Test
suite10 = test """Ampersand Decimal Interpolation — Decimals should interpolate seamlessly with proper significance.""" <|
    \_ ->
    (E.object
      [ ( "power"
        , (E.float 1.21)
        )
      ])
    |> render10
    |> equal """\"1.21 jiggawatts!\""""



{- Basic Null Interpolation
The template is:

---
I ({{cannot}}) be seen!
---
The data is:

---
{
    "cannot": null
}
---
-}

render11 : Value -> String
render11 json = (\context0 ->
    """I (""" ++ htmlEscape (interpolate (lookup context0 ["cannot"])) ++ """) be seen!""") [json]

suite11 : Test
suite11 = test """Basic Null Interpolation — Nulls should interpolate as the empty string.""" <|
    \_ ->
    (E.object
      [ ( "cannot"
        , E.null
        )
      ])
    |> render11
    |> equal """I () be seen!"""



{- Triple Mustache Null Interpolation
The template is:

---
I ({{{cannot}}}) be seen!
---
The data is:

---
{
    "cannot": null
}
---
-}

render12 : Value -> String
render12 json = (\context0 ->
    """I (""" ++ interpolate (lookup context0 ["cannot"]) ++ """) be seen!""") [json]

suite12 : Test
suite12 = test """Triple Mustache Null Interpolation — Nulls should interpolate as the empty string.""" <|
    \_ ->
    (E.object
      [ ( "cannot"
        , E.null
        )
      ])
    |> render12
    |> equal """I () be seen!"""



{- Ampersand Null Interpolation
The template is:

---
I ({{&cannot}}) be seen!
---
The data is:

---
{
    "cannot": null
}
---
-}

render13 : Value -> String
render13 json = (\context0 ->
    """I (""" ++ interpolate (lookup context0 ["cannot"]) ++ """) be seen!""") [json]

suite13 : Test
suite13 = test """Ampersand Null Interpolation — Nulls should interpolate as the empty string.""" <|
    \_ ->
    (E.object
      [ ( "cannot"
        , E.null
        )
      ])
    |> render13
    |> equal """I () be seen!"""



{- Basic Context Miss Interpolation
The template is:

---
I ({{cannot}}) be seen!
---
The data is:

---
{}
---
-}

render14 : Value -> String
render14 json = (\context0 ->
    """I (""" ++ htmlEscape (interpolate (lookup context0 ["cannot"])) ++ """) be seen!""") [json]

suite14 : Test
suite14 = test """Basic Context Miss Interpolation — Failed context lookups should default to empty strings.""" <|
    \_ ->
    (E.object [])
    |> render14
    |> equal """I () be seen!"""



{- Triple Mustache Context Miss Interpolation
The template is:

---
I ({{{cannot}}}) be seen!
---
The data is:

---
{}
---
-}

render15 : Value -> String
render15 json = (\context0 ->
    """I (""" ++ interpolate (lookup context0 ["cannot"]) ++ """) be seen!""") [json]

suite15 : Test
suite15 = test """Triple Mustache Context Miss Interpolation — Failed context lookups should default to empty strings.""" <|
    \_ ->
    (E.object [])
    |> render15
    |> equal """I () be seen!"""



{- Ampersand Context Miss Interpolation
The template is:

---
I ({{&cannot}}) be seen!
---
The data is:

---
{}
---
-}

render16 : Value -> String
render16 json = (\context0 ->
    """I (""" ++ interpolate (lookup context0 ["cannot"]) ++ """) be seen!""") [json]

suite16 : Test
suite16 = test """Ampersand Context Miss Interpolation — Failed context lookups should default to empty strings.""" <|
    \_ ->
    (E.object [])
    |> render16
    |> equal """I () be seen!"""



{- Dotted Names - Basic Interpolation
The template is:

---
"{{person.name}}" == "{{#person}}{{name}}{{/person}}"
---
The data is:

---
{
    "person": {
        "name": "Joe"
    }
}
---
-}

render17 : Value -> String
render17 json = (\context0 ->
    """\"""" ++ htmlEscape (interpolate (lookup context0 ["person", "name"])) ++ """\" == \"""" ++ (section context0 ["person"] (\context1 ->htmlEscape (interpolate (lookup context1 ["name"])))) ++ """\"""") [json]

suite17 : Test
suite17 = test """Dotted Names - Basic Interpolation — Dotted names should be considered a form of shorthand for sections.""" <|
    \_ ->
    (E.object
      [ ( "person"
        , (E.object
          [ ( "name"
            , (E.string """Joe""")
            )
          ])
        )
      ])
    |> render17
    |> equal """\"Joe\" == \"Joe\""""



{- Dotted Names - Triple Mustache Interpolation
The template is:

---
"{{{person.name}}}" == "{{#person}}{{{name}}}{{/person}}"
---
The data is:

---
{
    "person": {
        "name": "Joe"
    }
}
---
-}

render18 : Value -> String
render18 json = (\context0 ->
    """\"""" ++ interpolate (lookup context0 ["person", "name"]) ++ """\" == \"""" ++ (section context0 ["person"] (\context1 ->interpolate (lookup context1 ["name"]))) ++ """\"""") [json]

suite18 : Test
suite18 = test """Dotted Names - Triple Mustache Interpolation — Dotted names should be considered a form of shorthand for sections.""" <|
    \_ ->
    (E.object
      [ ( "person"
        , (E.object
          [ ( "name"
            , (E.string """Joe""")
            )
          ])
        )
      ])
    |> render18
    |> equal """\"Joe\" == \"Joe\""""



{- Dotted Names - Ampersand Interpolation
The template is:

---
"{{&person.name}}" == "{{#person}}{{&name}}{{/person}}"
---
The data is:

---
{
    "person": {
        "name": "Joe"
    }
}
---
-}

render19 : Value -> String
render19 json = (\context0 ->
    """\"""" ++ interpolate (lookup context0 ["person", "name"]) ++ """\" == \"""" ++ (section context0 ["person"] (\context1 ->interpolate (lookup context1 ["name"]))) ++ """\"""") [json]

suite19 : Test
suite19 = test """Dotted Names - Ampersand Interpolation — Dotted names should be considered a form of shorthand for sections.""" <|
    \_ ->
    (E.object
      [ ( "person"
        , (E.object
          [ ( "name"
            , (E.string """Joe""")
            )
          ])
        )
      ])
    |> render19
    |> equal """\"Joe\" == \"Joe\""""



{- Dotted Names - Arbitrary Depth
The template is:

---
"{{a.b.c.d.e.name}}" == "Phil"
---
The data is:

---
{
    "a": {
        "b": {
            "c": {
                "d": {
                    "e": {
                        "name": "Phil"
                    }
                }
            }
        }
    }
}
---
-}

render20 : Value -> String
render20 json = (\context0 ->
    """\"""" ++ htmlEscape (interpolate (lookup context0 ["a", "b", "c", "d", "e", "name"])) ++ """\" == \"Phil\"""") [json]

suite20 : Test
suite20 = test """Dotted Names - Arbitrary Depth — Dotted names should be functional to any level of nesting.""" <|
    \_ ->
    (E.object
      [ ( "a"
        , (E.object
          [ ( "b"
            , (E.object
              [ ( "c"
                , (E.object
                  [ ( "d"
                    , (E.object
                      [ ( "e"
                        , (E.object
                          [ ( "name"
                            , (E.string """Phil""")
                            )
                          ])
                        )
                      ])
                    )
                  ])
                )
              ])
            )
          ])
        )
      ])
    |> render20
    |> equal """\"Phil\" == \"Phil\""""



{- Dotted Names - Broken Chains
The template is:

---
"{{a.b.c}}" == ""
---
The data is:

---
{
    "a": {}
}
---
-}

render21 : Value -> String
render21 json = (\context0 ->
    """\"""" ++ htmlEscape (interpolate (lookup context0 ["a", "b", "c"])) ++ """\" == \"\"""") [json]

suite21 : Test
suite21 = test """Dotted Names - Broken Chains — Any falsey value prior to the last part of the name should yield ''.""" <|
    \_ ->
    (E.object
      [ ( "a"
        , (E.object [])
        )
      ])
    |> render21
    |> equal """\"\" == \"\""""



{- Dotted Names - Broken Chain Resolution
The template is:

---
"{{a.b.c.name}}" == ""
---
The data is:

---
{
    "a": {
        "b": {}
    },
    "c": {
        "name": "Jim"
    }
}
---
-}

render22 : Value -> String
render22 json = (\context0 ->
    """\"""" ++ htmlEscape (interpolate (lookup context0 ["a", "b", "c", "name"])) ++ """\" == \"\"""") [json]

suite22 : Test
suite22 = test """Dotted Names - Broken Chain Resolution — Each part of a dotted name should resolve only against its parent.""" <|
    \_ ->
    (E.object
      [ ( "a"
        , (E.object
          [ ( "b"
            , (E.object [])
            )
          ])
        )
      , ( "c"
        , (E.object
          [ ( "name"
            , (E.string """Jim""")
            )
          ])
        )
      ])
    |> render22
    |> equal """\"\" == \"\""""



{- Dotted Names - Initial Resolution
The template is:

---
"{{#a}}{{b.c.d.e.name}}{{/a}}" == "Phil"
---
The data is:

---
{
    "a": {
        "b": {
            "c": {
                "d": {
                    "e": {
                        "name": "Phil"
                    }
                }
            }
        }
    },
    "b": {
        "c": {
            "d": {
                "e": {
                    "name": "Wrong"
                }
            }
        }
    }
}
---
-}

render23 : Value -> String
render23 json = (\context0 ->
    """\"""" ++ (section context0 ["a"] (\context1 ->htmlEscape (interpolate (lookup context1 ["b", "c", "d", "e", "name"])))) ++ """\" == \"Phil\"""") [json]

suite23 : Test
suite23 = test """Dotted Names - Initial Resolution — The first part of a dotted name should resolve as any other name.""" <|
    \_ ->
    (E.object
      [ ( "a"
        , (E.object
          [ ( "b"
            , (E.object
              [ ( "c"
                , (E.object
                  [ ( "d"
                    , (E.object
                      [ ( "e"
                        , (E.object
                          [ ( "name"
                            , (E.string """Phil""")
                            )
                          ])
                        )
                      ])
                    )
                  ])
                )
              ])
            )
          ])
        )
      , ( "b"
        , (E.object
          [ ( "c"
            , (E.object
              [ ( "d"
                , (E.object
                  [ ( "e"
                    , (E.object
                      [ ( "name"
                        , (E.string """Wrong""")
                        )
                      ])
                    )
                  ])
                )
              ])
            )
          ])
        )
      ])
    |> render23
    |> equal """\"Phil\" == \"Phil\""""



{- Dotted Names - Context Precedence
The template is:

---
{{#a}}{{b.c}}{{/a}}
---
The data is:

---
{
    "a": {
        "b": {}
    },
    "b": {
        "c": "ERROR"
    }
}
---
-}

render24 : Value -> String
render24 json = (\context0 ->
    (section context0 ["a"] (\context1 ->htmlEscape (interpolate (lookup context1 ["b", "c"]))))) [json]

suite24 : Test
suite24 = test """Dotted Names - Context Precedence — Dotted names should be resolved against former resolutions.""" <|
    \_ ->
    (E.object
      [ ( "a"
        , (E.object
          [ ( "b"
            , (E.object [])
            )
          ])
        )
      , ( "b"
        , (E.object
          [ ( "c"
            , (E.string """ERROR""")
            )
          ])
        )
      ])
    |> render24
    |> equal """"""



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

render25 : Value -> String
render25 json = (\context0 ->
    """Hello, """ ++ htmlEscape (interpolate (lookup context0 [])) ++ """!
""") [json]

suite25 : Test
suite25 = test """Implicit Iterators - Basic Interpolation — Unadorned tags should interpolate content into the template.""" <|
    \_ ->
    (E.string """world""")
    |> render25
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

render26 : Value -> String
render26 json = (\context0 ->
    """These characters should be HTML escaped: """ ++ htmlEscape (interpolate (lookup context0 [])) ++ """
""") [json]

suite26 : Test
suite26 = test """Implicit Iterators - HTML Escaping — Basic interpolation should be HTML escaped.""" <|
    \_ ->
    (E.string """& \" < >""")
    |> render26
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

render27 : Value -> String
render27 json = (\context0 ->
    """These characters should not be HTML escaped: """ ++ interpolate (lookup context0 []) ++ """
""") [json]

suite27 : Test
suite27 = test """Implicit Iterators - Triple Mustache — Triple mustaches should interpolate without HTML escaping.""" <|
    \_ ->
    (E.string """& \" < >""")
    |> render27
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

render28 : Value -> String
render28 json = (\context0 ->
    """These characters should not be HTML escaped: """ ++ interpolate (lookup context0 []) ++ """
""") [json]

suite28 : Test
suite28 = test """Implicit Iterators - Ampersand — Ampersand should interpolate without HTML escaping.""" <|
    \_ ->
    (E.string """& \" < >""")
    |> render28
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

render29 : Value -> String
render29 json = (\context0 ->
    """\"""" ++ htmlEscape (interpolate (lookup context0 [])) ++ """ miles an hour!\"""") [json]

suite29 : Test
suite29 = test """Implicit Iterators - Basic Integer Interpolation — Integers should interpolate seamlessly.""" <|
    \_ ->
    (E.int 85)
    |> render29
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

render30 : Value -> String
render30 json = (\context0 ->
    """| """ ++ htmlEscape (interpolate (lookup context0 ["string"])) ++ """ |""") [json]

suite30 : Test
suite30 = test """Interpolation - Surrounding Whitespace — Interpolation should not alter surrounding whitespace.""" <|
    \_ ->
    (E.object
      [ ( "string"
        , (E.string """---""")
        )
      ])
    |> render30
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

render31 : Value -> String
render31 json = (\context0 ->
    """| """ ++ interpolate (lookup context0 ["string"]) ++ """ |""") [json]

suite31 : Test
suite31 = test """Triple Mustache - Surrounding Whitespace — Interpolation should not alter surrounding whitespace.""" <|
    \_ ->
    (E.object
      [ ( "string"
        , (E.string """---""")
        )
      ])
    |> render31
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

render32 : Value -> String
render32 json = (\context0 ->
    """| """ ++ interpolate (lookup context0 ["string"]) ++ """ |""") [json]

suite32 : Test
suite32 = test """Ampersand - Surrounding Whitespace — Interpolation should not alter surrounding whitespace.""" <|
    \_ ->
    (E.object
      [ ( "string"
        , (E.string """---""")
        )
      ])
    |> render32
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

render33 : Value -> String
render33 json = (\context0 ->
    """  """ ++ htmlEscape (interpolate (lookup context0 ["string"])) ++ """
""") [json]

suite33 : Test
suite33 = test """Interpolation - Standalone — Standalone interpolation should not alter surrounding whitespace.""" <|
    \_ ->
    (E.object
      [ ( "string"
        , (E.string """---""")
        )
      ])
    |> render33
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

render34 : Value -> String
render34 json = (\context0 ->
    """  """ ++ interpolate (lookup context0 ["string"]) ++ """
""") [json]

suite34 : Test
suite34 = test """Triple Mustache - Standalone — Standalone interpolation should not alter surrounding whitespace.""" <|
    \_ ->
    (E.object
      [ ( "string"
        , (E.string """---""")
        )
      ])
    |> render34
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

render35 : Value -> String
render35 json = (\context0 ->
    """  """ ++ interpolate (lookup context0 ["string"]) ++ """
""") [json]

suite35 : Test
suite35 = test """Ampersand - Standalone — Standalone interpolation should not alter surrounding whitespace.""" <|
    \_ ->
    (E.object
      [ ( "string"
        , (E.string """---""")
        )
      ])
    |> render35
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

render36 : Value -> String
render36 json = (\context0 ->
    """|""" ++ htmlEscape (interpolate (lookup context0 ["string"])) ++ """|""") [json]

suite36 : Test
suite36 = test """Interpolation With Padding — Superfluous in-tag whitespace should be ignored.""" <|
    \_ ->
    (E.object
      [ ( "string"
        , (E.string """---""")
        )
      ])
    |> render36
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

render37 : Value -> String
render37 json = (\context0 ->
    """|""" ++ interpolate (lookup context0 ["string"]) ++ """|""") [json]

suite37 : Test
suite37 = test """Triple Mustache With Padding — Superfluous in-tag whitespace should be ignored.""" <|
    \_ ->
    (E.object
      [ ( "string"
        , (E.string """---""")
        )
      ])
    |> render37
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

render38 : Value -> String
render38 json = (\context0 ->
    """|""" ++ interpolate (lookup context0 ["string"]) ++ """|""") [json]

suite38 : Test
suite38 = test """Ampersand With Padding — Superfluous in-tag whitespace should be ignored.""" <|
    \_ ->
    (E.object
      [ ( "string"
        , (E.string """---""")
        )
      ])
    |> render38
    |> equal """|---|"""

