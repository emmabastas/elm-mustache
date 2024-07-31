module PreparsedUnitTests.Inverted exposing (..)

import Json.Decode as D exposing (Value)
import Json.Encode as E
import Mustache exposing (htmlEscape, lookup, interpolate, section, invertedSection)

import Test exposing (Test, test)
import Expect exposing (equal)

comment : String
comment = ""

setDelimiter : String -> String -> String
setDelimiter _ _ = ""



{- Falsey
The template is:

---
"{{^boolean}}This should be rendered.{{/boolean}}"
---
The data is:

---
{
    "boolean": false
}
---
-}

render0 : Value -> String
render0 json = (\context0 ->
    """\"""" ++ (invertedSection context0 ["boolean"] (\context1 ->"""This should be rendered.""")) ++ """\"""") [json]

suite0 : Test
suite0 = test """Falsey — Falsey sections should have their contents rendered.""" <|
    \_ ->
    (E.object
      [ ( "boolean"
        , (E.bool False)
        )
      ])
    |> render0
    |> equal """\"This should be rendered.\""""



{- Truthy
The template is:

---
"{{^boolean}}This should not be rendered.{{/boolean}}"
---
The data is:

---
{
    "boolean": true
}
---
-}

render1 : Value -> String
render1 json = (\context0 ->
    """\"""" ++ (invertedSection context0 ["boolean"] (\context1 ->"""This should not be rendered.""")) ++ """\"""") [json]

suite1 : Test
suite1 = test """Truthy — Truthy sections should have their contents omitted.""" <|
    \_ ->
    (E.object
      [ ( "boolean"
        , (E.bool True)
        )
      ])
    |> render1
    |> equal """\"\""""



{- Null is falsey
The template is:

---
"{{^null}}This should be rendered.{{/null}}"
---
The data is:

---
{
    "null": null
}
---
-}

render2 : Value -> String
render2 json = (\context0 ->
    """\"""" ++ (invertedSection context0 ["null"] (\context1 ->"""This should be rendered.""")) ++ """\"""") [json]

suite2 : Test
suite2 = test """Null is falsey — Null is falsey.""" <|
    \_ ->
    (E.object
      [ ( "null"
        , E.null
        )
      ])
    |> render2
    |> equal """\"This should be rendered.\""""



{- Context
The template is:

---
"{{^context}}Hi {{name}}.{{/context}}"
---
The data is:

---
{
    "context": {
        "name": "Joe"
    }
}
---
-}

render3 : Value -> String
render3 json = (\context0 ->
    """\"""" ++ (invertedSection context0 ["context"] (\context1 ->"""Hi """ ++ htmlEscape (interpolate (lookup context1 ["name"])) ++ """.""")) ++ """\"""") [json]

suite3 : Test
suite3 = test """Context — Objects and hashes should behave like truthy values.""" <|
    \_ ->
    (E.object
      [ ( "context"
        , (E.object
          [ ( "name"
            , (E.string """Joe""")
            )
          ])
        )
      ])
    |> render3
    |> equal """\"\""""



{- List
The template is:

---
"{{^list}}{{n}}{{/list}}"
---
The data is:

---
{
    "list": [
        {
            "n": 1
        },
        {
            "n": 2
        },
        {
            "n": 3
        }
    ]
}
---
-}

render4 : Value -> String
render4 json = (\context0 ->
    """\"""" ++ (invertedSection context0 ["list"] (\context1 ->htmlEscape (interpolate (lookup context1 ["n"])))) ++ """\"""") [json]

suite4 : Test
suite4 = test """List — Lists should behave like truthy values.""" <|
    \_ ->
    (E.object
      [ ( "list"
        , (E.list identity [ (E.object
            [ ( "n"
              , (E.int 1)
              )
            ])
        , (E.object
            [ ( "n"
              , (E.int 2)
              )
            ])
        , (E.object
            [ ( "n"
              , (E.int 3)
              )
            ])
        ])
        )
      ])
    |> render4
    |> equal """\"\""""



{- Empty List
The template is:

---
"{{^list}}Yay lists!{{/list}}"
---
The data is:

---
{
    "list": []
}
---
-}

render5 : Value -> String
render5 json = (\context0 ->
    """\"""" ++ (invertedSection context0 ["list"] (\context1 ->"""Yay lists!""")) ++ """\"""") [json]

suite5 : Test
suite5 = test """Empty List — Empty lists should behave like falsey values.""" <|
    \_ ->
    (E.object
      [ ( "list"
        , (E.list identity [])
        )
      ])
    |> render5
    |> equal """\"Yay lists!\""""



{- Doubled
The template is:

---
{{^bool}}
* first
{{/bool}}
* {{two}}
{{^bool}}
* third
{{/bool}}

---
The data is:

---
{
    "bool": false,
    "two": "second"
}
---
-}

render6 : Value -> String
render6 json = (\context0 ->
    (invertedSection context0 ["bool"] (\context1 ->"""* first
""")) ++ """* """ ++ htmlEscape (interpolate (lookup context0 ["two"])) ++ """
""" ++ (invertedSection context0 ["bool"] (\context1 ->"""* third
"""))) [json]

suite6 : Test
suite6 = test """Doubled — Multiple inverted sections per template should be permitted.""" <|
    \_ ->
    (E.object
      [ ( "bool"
        , (E.bool False)
        )
      , ( "two"
        , (E.string """second""")
        )
      ])
    |> render6
    |> equal """* first
* second
* third
"""



{- Nested (Falsey)
The template is:

---
| A {{^bool}}B {{^bool}}C{{/bool}} D{{/bool}} E |
---
The data is:

---
{
    "bool": false
}
---
-}

render7 : Value -> String
render7 json = (\context0 ->
    """| A """ ++ (invertedSection context0 ["bool"] (\context1 ->"""B """ ++ (invertedSection context1 ["bool"] (\context2 ->"""C""")) ++ """ D""")) ++ """ E |""") [json]

suite7 : Test
suite7 = test """Nested (Falsey) — Nested falsey sections should have their contents rendered.""" <|
    \_ ->
    (E.object
      [ ( "bool"
        , (E.bool False)
        )
      ])
    |> render7
    |> equal """| A B C D E |"""



{- Nested (Truthy)
The template is:

---
| A {{^bool}}B {{^bool}}C{{/bool}} D{{/bool}} E |
---
The data is:

---
{
    "bool": true
}
---
-}

render8 : Value -> String
render8 json = (\context0 ->
    """| A """ ++ (invertedSection context0 ["bool"] (\context1 ->"""B """ ++ (invertedSection context1 ["bool"] (\context2 ->"""C""")) ++ """ D""")) ++ """ E |""") [json]

suite8 : Test
suite8 = test """Nested (Truthy) — Nested truthy sections should be omitted.""" <|
    \_ ->
    (E.object
      [ ( "bool"
        , (E.bool True)
        )
      ])
    |> render8
    |> equal """| A  E |"""



{- Context Misses
The template is:

---
[{{^missing}}Cannot find key 'missing'!{{/missing}}]
---
The data is:

---
{}
---
-}

render9 : Value -> String
render9 json = (\context0 ->
    """[""" ++ (invertedSection context0 ["missing"] (\context1 ->"""Cannot find key 'missing'!""")) ++ """]""") [json]

suite9 : Test
suite9 = test """Context Misses — Failed context lookups should be considered falsey.""" <|
    \_ ->
    (E.object [])
    |> render9
    |> equal """[Cannot find key 'missing'!]"""



{- Dotted Names - Truthy
The template is:

---
"{{^a.b.c}}Not Here{{/a.b.c}}" == ""
---
The data is:

---
{
    "a": {
        "b": {
            "c": true
        }
    }
}
---
-}

render10 : Value -> String
render10 json = (\context0 ->
    """\"""" ++ (invertedSection context0 ["a", "b", "c"] (\context1 ->"""Not Here""")) ++ """\" == \"\"""") [json]

suite10 : Test
suite10 = test """Dotted Names - Truthy — Dotted names should be valid for Inverted Section tags.""" <|
    \_ ->
    (E.object
      [ ( "a"
        , (E.object
          [ ( "b"
            , (E.object
              [ ( "c"
                , (E.bool True)
                )
              ])
            )
          ])
        )
      ])
    |> render10
    |> equal """\"\" == \"\""""



{- Dotted Names - Falsey
The template is:

---
"{{^a.b.c}}Not Here{{/a.b.c}}" == "Not Here"
---
The data is:

---
{
    "a": {
        "b": {
            "c": false
        }
    }
}
---
-}

render11 : Value -> String
render11 json = (\context0 ->
    """\"""" ++ (invertedSection context0 ["a", "b", "c"] (\context1 ->"""Not Here""")) ++ """\" == \"Not Here\"""") [json]

suite11 : Test
suite11 = test """Dotted Names - Falsey — Dotted names should be valid for Inverted Section tags.""" <|
    \_ ->
    (E.object
      [ ( "a"
        , (E.object
          [ ( "b"
            , (E.object
              [ ( "c"
                , (E.bool False)
                )
              ])
            )
          ])
        )
      ])
    |> render11
    |> equal """\"Not Here\" == \"Not Here\""""



{- Dotted Names - Broken Chains
The template is:

---
"{{^a.b.c}}Not Here{{/a.b.c}}" == "Not Here"
---
The data is:

---
{
    "a": {}
}
---
-}

render12 : Value -> String
render12 json = (\context0 ->
    """\"""" ++ (invertedSection context0 ["a", "b", "c"] (\context1 ->"""Not Here""")) ++ """\" == \"Not Here\"""") [json]

suite12 : Test
suite12 = test """Dotted Names - Broken Chains — Dotted names that cannot be resolved should be considered falsey.""" <|
    \_ ->
    (E.object
      [ ( "a"
        , (E.object [])
        )
      ])
    |> render12
    |> equal """\"Not Here\" == \"Not Here\""""



{- Surrounding Whitespace
The template is:

---
 | {{^boolean}}\t|\t{{/boolean}} | 

---
The data is:

---
{
    "boolean": false
}
---
-}

render13 : Value -> String
render13 json = (\context0 ->
    """ | """ ++ (invertedSection context0 ["boolean"] (\context1 ->"""	|	""")) ++ """ | 
""") [json]

suite13 : Test
suite13 = test """Surrounding Whitespace — Inverted sections should not alter surrounding whitespace.""" <|
    \_ ->
    (E.object
      [ ( "boolean"
        , (E.bool False)
        )
      ])
    |> render13
    |> equal """ | 	|	 | 
"""



{- Internal Whitespace
The template is:

---
 | {{^boolean}} {{! Important Whitespace }}
 {{/boolean}} | 

---
The data is:

---
{
    "boolean": false
}
---
-}

render14 : Value -> String
render14 json = (\context0 ->
    """ | """ ++ (invertedSection context0 ["boolean"] (\context1 ->""" """ ++ comment ++ """
 """)) ++ """ | 
""") [json]

suite14 : Test
suite14 = test """Internal Whitespace — Inverted should not alter internal whitespace.""" <|
    \_ ->
    (E.object
      [ ( "boolean"
        , (E.bool False)
        )
      ])
    |> render14
    |> equal """ |  
  | 
"""



{- Indented Inline Sections
The template is:

---
 {{^boolean}}NO{{/boolean}}
 {{^boolean}}WAY{{/boolean}}

---
The data is:

---
{
    "boolean": false
}
---
-}

render15 : Value -> String
render15 json = (\context0 ->
    """ """ ++ (invertedSection context0 ["boolean"] (\context1 ->"""NO""")) ++ """
 """ ++ (invertedSection context0 ["boolean"] (\context1 ->"""WAY""")) ++ """
""") [json]

suite15 : Test
suite15 = test """Indented Inline Sections — Single-line sections should not alter surrounding whitespace.""" <|
    \_ ->
    (E.object
      [ ( "boolean"
        , (E.bool False)
        )
      ])
    |> render15
    |> equal """ NO
 WAY
"""



{- Standalone Lines
The template is:

---
| This Is
{{^boolean}}
|
{{/boolean}}
| A Line

---
The data is:

---
{
    "boolean": false
}
---
-}

render16 : Value -> String
render16 json = (\context0 ->
    """| This Is
""" ++ (invertedSection context0 ["boolean"] (\context1 ->"""|
""")) ++ """| A Line
""") [json]

suite16 : Test
suite16 = test """Standalone Lines — Standalone lines should be removed from the template.""" <|
    \_ ->
    (E.object
      [ ( "boolean"
        , (E.bool False)
        )
      ])
    |> render16
    |> equal """| This Is
|
| A Line
"""



{- Standalone Indented Lines
The template is:

---
| This Is
  {{^boolean}}
|
  {{/boolean}}
| A Line

---
The data is:

---
{
    "boolean": false
}
---
-}

render17 : Value -> String
render17 json = (\context0 ->
    """| This Is
""" ++ (invertedSection context0 ["boolean"] (\context1 ->"""|
""")) ++ """| A Line
""") [json]

suite17 : Test
suite17 = test """Standalone Indented Lines — Standalone indented lines should be removed from the template.""" <|
    \_ ->
    (E.object
      [ ( "boolean"
        , (E.bool False)
        )
      ])
    |> render17
    |> equal """| This Is
|
| A Line
"""



{- Standalone Line Endings
The template is:

---
|
{{^boolean}}
{{/boolean}}
|
---
The data is:

---
{
    "boolean": false
}
---
-}

render18 : Value -> String
render18 json = (\context0 ->
    """|
""" ++ (invertedSection context0 ["boolean"] (\context1 ->"""""")) ++ """|""") [json]

suite18 : Test
suite18 = test """Standalone Line Endings — "\r\n" should be considered a newline for standalone tags.""" <|
    \_ ->
    (E.object
      [ ( "boolean"
        , (E.bool False)
        )
      ])
    |> render18
    |> equal """|
|"""



{- Standalone Without Previous Line
The template is:

---
  {{^boolean}}
^{{/boolean}}
/
---
The data is:

---
{
    "boolean": false
}
---
-}

render19 : Value -> String
render19 json = (\context0 ->
    (invertedSection context0 ["boolean"] (\context1 ->"""^""")) ++ """
/""") [json]

suite19 : Test
suite19 = test """Standalone Without Previous Line — Standalone tags should not require a newline to precede them.""" <|
    \_ ->
    (E.object
      [ ( "boolean"
        , (E.bool False)
        )
      ])
    |> render19
    |> equal """^
/"""



{- Standalone Without Newline
The template is:

---
^{{^boolean}}
/
  {{/boolean}}
---
The data is:

---
{
    "boolean": false
}
---
-}

render20 : Value -> String
render20 json = (\context0 ->
    """^""" ++ (invertedSection context0 ["boolean"] (\context1 ->"""
/
"""))) [json]

suite20 : Test
suite20 = test """Standalone Without Newline — Standalone tags should not require a newline to follow them.""" <|
    \_ ->
    (E.object
      [ ( "boolean"
        , (E.bool False)
        )
      ])
    |> render20
    |> equal """^
/
"""



{- Padding
The template is:

---
|{{^ boolean }}={{/ boolean }}|
---
The data is:

---
{
    "boolean": false
}
---
-}

render21 : Value -> String
render21 json = (\context0 ->
    """|""" ++ (invertedSection context0 ["boolean"] (\context1 ->"""=""")) ++ """|""") [json]

suite21 : Test
suite21 = test """Padding — Superfluous in-tag whitespace should be ignored.""" <|
    \_ ->
    (E.object
      [ ( "boolean"
        , (E.bool False)
        )
      ])
    |> render21
    |> equal """|=|"""

