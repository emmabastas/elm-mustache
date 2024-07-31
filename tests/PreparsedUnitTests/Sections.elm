module PreparsedUnitTests.Sections exposing (..)

import Json.Decode as D exposing (Value)
import Json.Encode as E
import Mustache exposing (htmlEscape, lookup, interpolate, section, invertedSection)

import Test exposing (Test, test)
import Expect exposing (equal)

comment : String
comment = ""

setDelimiter : String -> String -> String
setDelimiter _ _ = ""



{- Truthy
The template is:

---
"{{#boolean}}This should be rendered.{{/boolean}}"
---
The data is:

---
{
    "boolean": true
}
---
-}

render0 : Value -> String
render0 json = (\context0 ->
    """\"""" ++ (section context0 ["boolean"] (\context1 ->"""This should be rendered.""")) ++ """\"""") [json]

suite0 : Test
suite0 = test """Truthy — Truthy sections should have their contents rendered.""" <|
    \_ ->
    (E.object
      [ ( "boolean"
        , (E.bool True)
        )
      ])
    |> render0
    |> equal """\"This should be rendered.\""""



{- Falsey
The template is:

---
"{{#boolean}}This should not be rendered.{{/boolean}}"
---
The data is:

---
{
    "boolean": false
}
---
-}

render1 : Value -> String
render1 json = (\context0 ->
    """\"""" ++ (section context0 ["boolean"] (\context1 ->"""This should not be rendered.""")) ++ """\"""") [json]

suite1 : Test
suite1 = test """Falsey — Falsey sections should have their contents omitted.""" <|
    \_ ->
    (E.object
      [ ( "boolean"
        , (E.bool False)
        )
      ])
    |> render1
    |> equal """\"\""""



{- Null is falsey
The template is:

---
"{{#null}}This should not be rendered.{{/null}}"
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
    """\"""" ++ (section context0 ["null"] (\context1 ->"""This should not be rendered.""")) ++ """\"""") [json]

suite2 : Test
suite2 = test """Null is falsey — Null is falsey.""" <|
    \_ ->
    (E.object
      [ ( "null"
        , E.null
        )
      ])
    |> render2
    |> equal """\"\""""



{- Context
The template is:

---
"{{#context}}Hi {{name}}.{{/context}}"
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
    """\"""" ++ (section context0 ["context"] (\context1 ->"""Hi """ ++ htmlEscape (interpolate (lookup context1 ["name"])) ++ """.""")) ++ """\"""") [json]

suite3 : Test
suite3 = test """Context — Objects and hashes should be pushed onto the context stack.""" <|
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
    |> equal """\"Hi Joe.\""""



{- Parent contexts
The template is:

---
"{{#sec}}{{a}}, {{b}}, {{c.d}}{{/sec}}"
---
The data is:

---
{
    "a": "foo",
    "b": "wrong",
    "sec": {
        "b": "bar"
    },
    "c": {
        "d": "baz"
    }
}
---
-}

render4 : Value -> String
render4 json = (\context0 ->
    """\"""" ++ (section context0 ["sec"] (\context1 ->htmlEscape (interpolate (lookup context1 ["a"])) ++ """, """ ++ htmlEscape (interpolate (lookup context1 ["b"])) ++ """, """ ++ htmlEscape (interpolate (lookup context1 ["c", "d"])))) ++ """\"""") [json]

suite4 : Test
suite4 = test """Parent contexts — Names missing in the current context are looked up in the stack.""" <|
    \_ ->
    (E.object
      [ ( "a"
        , (E.string """foo""")
        )
      , ( "b"
        , (E.string """wrong""")
        )
      , ( "sec"
        , (E.object
          [ ( "b"
            , (E.string """bar""")
            )
          ])
        )
      , ( "c"
        , (E.object
          [ ( "d"
            , (E.string """baz""")
            )
          ])
        )
      ])
    |> render4
    |> equal """\"foo, bar, baz\""""



{- Variable test
The template is:

---
"{{#foo}}{{.}} is {{foo}}{{/foo}}"
---
The data is:

---
{
    "foo": "bar"
}
---
-}

render5 : Value -> String
render5 json = (\context0 ->
    """\"""" ++ (section context0 ["foo"] (\context1 ->htmlEscape (interpolate (lookup context1 [])) ++ """ is """ ++ htmlEscape (interpolate (lookup context1 ["foo"])))) ++ """\"""") [json]

suite5 : Test
suite5 = test """Variable test — Non-false sections have their value at the top of context,
accessible as {{.}} or through the parent context. This gives
a simple way to display content conditionally if a variable exists.
""" <|
    \_ ->
    (E.object
      [ ( "foo"
        , (E.string """bar""")
        )
      ])
    |> render5
    |> equal """\"bar is bar\""""



{- List Contexts
The template is:

---
{{#tops}}{{#middles}}{{tname.lower}}{{mname}}.{{#bottoms}}{{tname.upper}}{{mname}}{{bname}}.{{/bottoms}}{{/middles}}{{/tops}}
---
The data is:

---
{
    "tops": [
        {
            "tname": {
                "upper": "A",
                "lower": "a"
            },
            "middles": [
                {
                    "mname": "1",
                    "bottoms": [
                        {
                            "bname": "x"
                        },
                        {
                            "bname": "y"
                        }
                    ]
                }
            ]
        }
    ]
}
---
-}

render6 : Value -> String
render6 json = (\context0 ->
    (section context0 ["tops"] (\context1 ->(section context1 ["middles"] (\context2 ->htmlEscape (interpolate (lookup context2 ["tname", "lower"])) ++ htmlEscape (interpolate (lookup context2 ["mname"])) ++ """.""" ++ (section context2 ["bottoms"] (\context3 ->htmlEscape (interpolate (lookup context3 ["tname", "upper"])) ++ htmlEscape (interpolate (lookup context3 ["mname"])) ++ htmlEscape (interpolate (lookup context3 ["bname"])) ++ """."""))))))) [json]

suite6 : Test
suite6 = test """List Contexts — All elements on the context stack should be accessible within lists.""" <|
    \_ ->
    (E.object
      [ ( "tops"
        , (E.list identity [(E.object
            [ ( "tname"
              , (E.object
                [ ( "upper"
                  , (E.string """A""")
                  )
                , ( "lower"
                  , (E.string """a""")
                  )
                ])
              )
            , ( "middles"
              , (E.list identity [(E.object
                  [ ( "mname"
                    , (E.string """1""")
                    )
                  , ( "bottoms"
                    , (E.list identity [ (E.object
                        [ ( "bname"
                          , (E.string """x""")
                          )
                        ])
                    , (E.object
                        [ ( "bname"
                          , (E.string """y""")
                          )
                        ])
                    ])
                    )
                  ])])
              )
            ])])
        )
      ])
    |> render6
    |> equal """a1.A1x.A1y."""



{- Deeply Nested Contexts
The template is:

---
{{#a}}
{{one}}
{{#b}}
{{one}}{{two}}{{one}}
{{#c}}
{{one}}{{two}}{{three}}{{two}}{{one}}
{{#d}}
{{one}}{{two}}{{three}}{{four}}{{three}}{{two}}{{one}}
{{#five}}
{{one}}{{two}}{{three}}{{four}}{{five}}{{four}}{{three}}{{two}}{{one}}
{{one}}{{two}}{{three}}{{four}}{{.}}6{{.}}{{four}}{{three}}{{two}}{{one}}
{{one}}{{two}}{{three}}{{four}}{{five}}{{four}}{{three}}{{two}}{{one}}
{{/five}}
{{one}}{{two}}{{three}}{{four}}{{three}}{{two}}{{one}}
{{/d}}
{{one}}{{two}}{{three}}{{two}}{{one}}
{{/c}}
{{one}}{{two}}{{one}}
{{/b}}
{{one}}
{{/a}}

---
The data is:

---
{
    "a": {
        "one": 1
    },
    "b": {
        "two": 2
    },
    "c": {
        "three": 3,
        "d": {
            "four": 4,
            "five": 5
        }
    }
}
---
-}

render7 : Value -> String
render7 json = (\context0 ->
    (section context0 ["a"] (\context1 ->htmlEscape (interpolate (lookup context1 ["one"])) ++ """
""" ++ (section context1 ["b"] (\context2 ->htmlEscape (interpolate (lookup context2 ["one"])) ++ htmlEscape (interpolate (lookup context2 ["two"])) ++ htmlEscape (interpolate (lookup context2 ["one"])) ++ """
""" ++ (section context2 ["c"] (\context3 ->htmlEscape (interpolate (lookup context3 ["one"])) ++ htmlEscape (interpolate (lookup context3 ["two"])) ++ htmlEscape (interpolate (lookup context3 ["three"])) ++ htmlEscape (interpolate (lookup context3 ["two"])) ++ htmlEscape (interpolate (lookup context3 ["one"])) ++ """
""" ++ (section context3 ["d"] (\context4 ->htmlEscape (interpolate (lookup context4 ["one"])) ++ htmlEscape (interpolate (lookup context4 ["two"])) ++ htmlEscape (interpolate (lookup context4 ["three"])) ++ htmlEscape (interpolate (lookup context4 ["four"])) ++ htmlEscape (interpolate (lookup context4 ["three"])) ++ htmlEscape (interpolate (lookup context4 ["two"])) ++ htmlEscape (interpolate (lookup context4 ["one"])) ++ """
""" ++ (section context4 ["five"] (\context5 ->htmlEscape (interpolate (lookup context5 ["one"])) ++ htmlEscape (interpolate (lookup context5 ["two"])) ++ htmlEscape (interpolate (lookup context5 ["three"])) ++ htmlEscape (interpolate (lookup context5 ["four"])) ++ htmlEscape (interpolate (lookup context5 ["five"])) ++ htmlEscape (interpolate (lookup context5 ["four"])) ++ htmlEscape (interpolate (lookup context5 ["three"])) ++ htmlEscape (interpolate (lookup context5 ["two"])) ++ htmlEscape (interpolate (lookup context5 ["one"])) ++ """
""" ++ htmlEscape (interpolate (lookup context5 ["one"])) ++ htmlEscape (interpolate (lookup context5 ["two"])) ++ htmlEscape (interpolate (lookup context5 ["three"])) ++ htmlEscape (interpolate (lookup context5 ["four"])) ++ htmlEscape (interpolate (lookup context5 [])) ++ """6""" ++ htmlEscape (interpolate (lookup context5 [])) ++ htmlEscape (interpolate (lookup context5 ["four"])) ++ htmlEscape (interpolate (lookup context5 ["three"])) ++ htmlEscape (interpolate (lookup context5 ["two"])) ++ htmlEscape (interpolate (lookup context5 ["one"])) ++ """
""" ++ htmlEscape (interpolate (lookup context5 ["one"])) ++ htmlEscape (interpolate (lookup context5 ["two"])) ++ htmlEscape (interpolate (lookup context5 ["three"])) ++ htmlEscape (interpolate (lookup context5 ["four"])) ++ htmlEscape (interpolate (lookup context5 ["five"])) ++ htmlEscape (interpolate (lookup context5 ["four"])) ++ htmlEscape (interpolate (lookup context5 ["three"])) ++ htmlEscape (interpolate (lookup context5 ["two"])) ++ htmlEscape (interpolate (lookup context5 ["one"])) ++ """
""")) ++ htmlEscape (interpolate (lookup context4 ["one"])) ++ htmlEscape (interpolate (lookup context4 ["two"])) ++ htmlEscape (interpolate (lookup context4 ["three"])) ++ htmlEscape (interpolate (lookup context4 ["four"])) ++ htmlEscape (interpolate (lookup context4 ["three"])) ++ htmlEscape (interpolate (lookup context4 ["two"])) ++ htmlEscape (interpolate (lookup context4 ["one"])) ++ """
""")) ++ htmlEscape (interpolate (lookup context3 ["one"])) ++ htmlEscape (interpolate (lookup context3 ["two"])) ++ htmlEscape (interpolate (lookup context3 ["three"])) ++ htmlEscape (interpolate (lookup context3 ["two"])) ++ htmlEscape (interpolate (lookup context3 ["one"])) ++ """
""")) ++ htmlEscape (interpolate (lookup context2 ["one"])) ++ htmlEscape (interpolate (lookup context2 ["two"])) ++ htmlEscape (interpolate (lookup context2 ["one"])) ++ """
""")) ++ htmlEscape (interpolate (lookup context1 ["one"])) ++ """
"""))) [json]

suite7 : Test
suite7 = test """Deeply Nested Contexts — All elements on the context stack should be accessible.""" <|
    \_ ->
    (E.object
      [ ( "a"
        , (E.object
          [ ( "one"
            , (E.int 1)
            )
          ])
        )
      , ( "b"
        , (E.object
          [ ( "two"
            , (E.int 2)
            )
          ])
        )
      , ( "c"
        , (E.object
          [ ( "three"
            , (E.int 3)
            )
          , ( "d"
            , (E.object
              [ ( "four"
                , (E.int 4)
                )
              , ( "five"
                , (E.int 5)
                )
              ])
            )
          ])
        )
      ])
    |> render7
    |> equal """1
121
12321
1234321
123454321
12345654321
123454321
1234321
12321
121
1
"""



{- List
The template is:

---
"{{#list}}{{item}}{{/list}}"
---
The data is:

---
{
    "list": [
        {
            "item": 1
        },
        {
            "item": 2
        },
        {
            "item": 3
        }
    ]
}
---
-}

render8 : Value -> String
render8 json = (\context0 ->
    """\"""" ++ (section context0 ["list"] (\context1 ->htmlEscape (interpolate (lookup context1 ["item"])))) ++ """\"""") [json]

suite8 : Test
suite8 = test """List — Lists should be iterated; list items should visit the context stack.""" <|
    \_ ->
    (E.object
      [ ( "list"
        , (E.list identity [ (E.object
            [ ( "item"
              , (E.int 1)
              )
            ])
        , (E.object
            [ ( "item"
              , (E.int 2)
              )
            ])
        , (E.object
            [ ( "item"
              , (E.int 3)
              )
            ])
        ])
        )
      ])
    |> render8
    |> equal """\"123\""""



{- Empty List
The template is:

---
"{{#list}}Yay lists!{{/list}}"
---
The data is:

---
{
    "list": []
}
---
-}

render9 : Value -> String
render9 json = (\context0 ->
    """\"""" ++ (section context0 ["list"] (\context1 ->"""Yay lists!""")) ++ """\"""") [json]

suite9 : Test
suite9 = test """Empty List — Empty lists should behave like falsey values.""" <|
    \_ ->
    (E.object
      [ ( "list"
        , (E.list identity [])
        )
      ])
    |> render9
    |> equal """\"\""""



{- Doubled
The template is:

---
{{#bool}}
* first
{{/bool}}
* {{two}}
{{#bool}}
* third
{{/bool}}

---
The data is:

---
{
    "bool": true,
    "two": "second"
}
---
-}

render10 : Value -> String
render10 json = (\context0 ->
    (section context0 ["bool"] (\context1 ->"""* first
""")) ++ """* """ ++ htmlEscape (interpolate (lookup context0 ["two"])) ++ """
""" ++ (section context0 ["bool"] (\context1 ->"""* third
"""))) [json]

suite10 : Test
suite10 = test """Doubled — Multiple sections per template should be permitted.""" <|
    \_ ->
    (E.object
      [ ( "bool"
        , (E.bool True)
        )
      , ( "two"
        , (E.string """second""")
        )
      ])
    |> render10
    |> equal """* first
* second
* third
"""



{- Nested (Truthy)
The template is:

---
| A {{#bool}}B {{#bool}}C{{/bool}} D{{/bool}} E |
---
The data is:

---
{
    "bool": true
}
---
-}

render11 : Value -> String
render11 json = (\context0 ->
    """| A """ ++ (section context0 ["bool"] (\context1 ->"""B """ ++ (section context1 ["bool"] (\context2 ->"""C""")) ++ """ D""")) ++ """ E |""") [json]

suite11 : Test
suite11 = test """Nested (Truthy) — Nested truthy sections should have their contents rendered.""" <|
    \_ ->
    (E.object
      [ ( "bool"
        , (E.bool True)
        )
      ])
    |> render11
    |> equal """| A B C D E |"""



{- Nested (Falsey)
The template is:

---
| A {{#bool}}B {{#bool}}C{{/bool}} D{{/bool}} E |
---
The data is:

---
{
    "bool": false
}
---
-}

render12 : Value -> String
render12 json = (\context0 ->
    """| A """ ++ (section context0 ["bool"] (\context1 ->"""B """ ++ (section context1 ["bool"] (\context2 ->"""C""")) ++ """ D""")) ++ """ E |""") [json]

suite12 : Test
suite12 = test """Nested (Falsey) — Nested falsey sections should be omitted.""" <|
    \_ ->
    (E.object
      [ ( "bool"
        , (E.bool False)
        )
      ])
    |> render12
    |> equal """| A  E |"""



{- Context Misses
The template is:

---
[{{#missing}}Found key 'missing'!{{/missing}}]
---
The data is:

---
{}
---
-}

render13 : Value -> String
render13 json = (\context0 ->
    """[""" ++ (section context0 ["missing"] (\context1 ->"""Found key 'missing'!""")) ++ """]""") [json]

suite13 : Test
suite13 = test """Context Misses — Failed context lookups should be considered falsey.""" <|
    \_ ->
    (E.object [])
    |> render13
    |> equal """[]"""



{- Implicit Iterator - String
The template is:

---
"{{#list}}({{.}}){{/list}}"
---
The data is:

---
{
    "list": [
        "a",
        "b",
        "c",
        "d",
        "e"
    ]
}
---
-}

render14 : Value -> String
render14 json = (\context0 ->
    """\"""" ++ (section context0 ["list"] (\context1 ->"""(""" ++ htmlEscape (interpolate (lookup context1 [])) ++ """)""")) ++ """\"""") [json]

suite14 : Test
suite14 = test """Implicit Iterator - String — Implicit iterators should directly interpolate strings.""" <|
    \_ ->
    (E.object
      [ ( "list"
        , (E.list identity [ (E.string """a""")
        , (E.string """b""")
        , (E.string """c""")
        , (E.string """d""")
        , (E.string """e""")
        ])
        )
      ])
    |> render14
    |> equal """\"(a)(b)(c)(d)(e)\""""



{- Implicit Iterator - Integer
The template is:

---
"{{#list}}({{.}}){{/list}}"
---
The data is:

---
{
    "list": [
        1,
        2,
        3,
        4,
        5
    ]
}
---
-}

render15 : Value -> String
render15 json = (\context0 ->
    """\"""" ++ (section context0 ["list"] (\context1 ->"""(""" ++ htmlEscape (interpolate (lookup context1 [])) ++ """)""")) ++ """\"""") [json]

suite15 : Test
suite15 = test """Implicit Iterator - Integer — Implicit iterators should cast integers to strings and interpolate.""" <|
    \_ ->
    (E.object
      [ ( "list"
        , (E.list identity [ (E.int 1)
        , (E.int 2)
        , (E.int 3)
        , (E.int 4)
        , (E.int 5)
        ])
        )
      ])
    |> render15
    |> equal """\"(1)(2)(3)(4)(5)\""""



{- Implicit Iterator - Decimal
The template is:

---
"{{#list}}({{.}}){{/list}}"
---
The data is:

---
{
    "list": [
        1.1,
        2.2,
        3.3,
        4.4,
        5.5
    ]
}
---
-}

render16 : Value -> String
render16 json = (\context0 ->
    """\"""" ++ (section context0 ["list"] (\context1 ->"""(""" ++ htmlEscape (interpolate (lookup context1 [])) ++ """)""")) ++ """\"""") [json]

suite16 : Test
suite16 = test """Implicit Iterator - Decimal — Implicit iterators should cast decimals to strings and interpolate.""" <|
    \_ ->
    (E.object
      [ ( "list"
        , (E.list identity [ (E.float 1.1)
        , (E.float 2.2)
        , (E.float 3.3)
        , (E.float 4.4)
        , (E.float 5.5)
        ])
        )
      ])
    |> render16
    |> equal """\"(1.1)(2.2)(3.3)(4.4)(5.5)\""""



{- Implicit Iterator - Array
The template is:

---
"{{#list}}({{#.}}{{.}}{{/.}}){{/list}}"
---
The data is:

---
{
    "list": [
        [
            1,
            2,
            3
        ],
        [
            "a",
            "b",
            "c"
        ]
    ]
}
---
-}

render17 : Value -> String
render17 json = (\context0 ->
    """\"""" ++ (section context0 ["list"] (\context1 ->"""(""" ++ (section context1 [] (\context2 ->htmlEscape (interpolate (lookup context2 [])))) ++ """)""")) ++ """\"""") [json]

suite17 : Test
suite17 = test """Implicit Iterator - Array — Implicit iterators should allow iterating over nested arrays.""" <|
    \_ ->
    (E.object
      [ ( "list"
        , (E.list identity [ (E.list identity [ (E.int 1)
          , (E.int 2)
          , (E.int 3)
          ])
        , (E.list identity [ (E.string """a""")
          , (E.string """b""")
          , (E.string """c""")
          ])
        ])
        )
      ])
    |> render17
    |> equal """\"(123)(abc)\""""



{- Implicit Iterator - HTML Escaping
The template is:

---
"{{#list}}({{.}}){{/list}}"
---
The data is:

---
{
    "list": [
        "&",
        "\"",
        "<",
        ">"
    ]
}
---
-}

render18 : Value -> String
render18 json = (\context0 ->
    """\"""" ++ (section context0 ["list"] (\context1 ->"""(""" ++ htmlEscape (interpolate (lookup context1 [])) ++ """)""")) ++ """\"""") [json]

suite18 : Test
suite18 = test """Implicit Iterator - HTML Escaping — Implicit iterators with basic interpolation should be HTML escaped.""" <|
    \_ ->
    (E.object
      [ ( "list"
        , (E.list identity [ (E.string """&""")
        , (E.string """\"""")
        , (E.string """<""")
        , (E.string """>""")
        ])
        )
      ])
    |> render18
    |> equal """\"(&amp;)(&quot;)(&lt;)(&gt;)\""""



{- Implicit Iterator - Triple mustache
The template is:

---
"{{#list}}({{{.}}}){{/list}}"
---
The data is:

---
{
    "list": [
        "&",
        "\"",
        "<",
        ">"
    ]
}
---
-}

render19 : Value -> String
render19 json = (\context0 ->
    """\"""" ++ (section context0 ["list"] (\context1 ->"""(""" ++ interpolate (lookup context1 []) ++ """)""")) ++ """\"""") [json]

suite19 : Test
suite19 = test """Implicit Iterator - Triple mustache — Implicit iterators in triple mustache should interpolate without HTML escaping.""" <|
    \_ ->
    (E.object
      [ ( "list"
        , (E.list identity [ (E.string """&""")
        , (E.string """\"""")
        , (E.string """<""")
        , (E.string """>""")
        ])
        )
      ])
    |> render19
    |> equal """\"(&)(\")(<)(>)\""""



{- Implicit Iterator - Ampersand
The template is:

---
"{{#list}}({{&.}}){{/list}}"
---
The data is:

---
{
    "list": [
        "&",
        "\"",
        "<",
        ">"
    ]
}
---
-}

render20 : Value -> String
render20 json = (\context0 ->
    """\"""" ++ (section context0 ["list"] (\context1 ->"""(""" ++ interpolate (lookup context1 []) ++ """)""")) ++ """\"""") [json]

suite20 : Test
suite20 = test """Implicit Iterator - Ampersand — Implicit iterators in an Ampersand tag should interpolate without HTML escaping.""" <|
    \_ ->
    (E.object
      [ ( "list"
        , (E.list identity [ (E.string """&""")
        , (E.string """\"""")
        , (E.string """<""")
        , (E.string """>""")
        ])
        )
      ])
    |> render20
    |> equal """\"(&)(\")(<)(>)\""""



{- Implicit Iterator - Root-level
The template is:

---
"{{#.}}({{value}}){{/.}}"
---
The data is:

---
[
    {
        "value": "a"
    },
    {
        "value": "b"
    }
]
---
-}

render21 : Value -> String
render21 json = (\context0 ->
    """\"""" ++ (section context0 [] (\context1 ->"""(""" ++ htmlEscape (interpolate (lookup context1 ["value"])) ++ """)""")) ++ """\"""") [json]

suite21 : Test
suite21 = test """Implicit Iterator - Root-level — Implicit iterators should work on root-level lists.""" <|
    \_ ->
    (E.list identity [ (E.object
        [ ( "value"
          , (E.string """a""")
          )
        ])
    , (E.object
        [ ( "value"
          , (E.string """b""")
          )
        ])
    ])
    |> render21
    |> equal """\"(a)(b)\""""



{- Dotted Names - Truthy
The template is:

---
"{{#a.b.c}}Here{{/a.b.c}}" == "Here"
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

render22 : Value -> String
render22 json = (\context0 ->
    """\"""" ++ (section context0 ["a", "b", "c"] (\context1 ->"""Here""")) ++ """\" == \"Here\"""") [json]

suite22 : Test
suite22 = test """Dotted Names - Truthy — Dotted names should be valid for Section tags.""" <|
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
    |> render22
    |> equal """\"Here\" == \"Here\""""



{- Dotted Names - Falsey
The template is:

---
"{{#a.b.c}}Here{{/a.b.c}}" == ""
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

render23 : Value -> String
render23 json = (\context0 ->
    """\"""" ++ (section context0 ["a", "b", "c"] (\context1 ->"""Here""")) ++ """\" == \"\"""") [json]

suite23 : Test
suite23 = test """Dotted Names - Falsey — Dotted names should be valid for Section tags.""" <|
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
    |> render23
    |> equal """\"\" == \"\""""



{- Dotted Names - Broken Chains
The template is:

---
"{{#a.b.c}}Here{{/a.b.c}}" == ""
---
The data is:

---
{
    "a": {}
}
---
-}

render24 : Value -> String
render24 json = (\context0 ->
    """\"""" ++ (section context0 ["a", "b", "c"] (\context1 ->"""Here""")) ++ """\" == \"\"""") [json]

suite24 : Test
suite24 = test """Dotted Names - Broken Chains — Dotted names that cannot be resolved should be considered falsey.""" <|
    \_ ->
    (E.object
      [ ( "a"
        , (E.object [])
        )
      ])
    |> render24
    |> equal """\"\" == \"\""""



{- Surrounding Whitespace
The template is:

---
 | {{#boolean}}\t|\t{{/boolean}} | 

---
The data is:

---
{
    "boolean": true
}
---
-}

render25 : Value -> String
render25 json = (\context0 ->
    """ | """ ++ (section context0 ["boolean"] (\context1 ->"""	|	""")) ++ """ | 
""") [json]

suite25 : Test
suite25 = test """Surrounding Whitespace — Sections should not alter surrounding whitespace.""" <|
    \_ ->
    (E.object
      [ ( "boolean"
        , (E.bool True)
        )
      ])
    |> render25
    |> equal """ | 	|	 | 
"""



{- Internal Whitespace
The template is:

---
 | {{#boolean}} {{! Important Whitespace }}
 {{/boolean}} | 

---
The data is:

---
{
    "boolean": true
}
---
-}

render26 : Value -> String
render26 json = (\context0 ->
    """ | """ ++ (section context0 ["boolean"] (\context1 ->""" """ ++ comment ++ """
 """)) ++ """ | 
""") [json]

suite26 : Test
suite26 = test """Internal Whitespace — Sections should not alter internal whitespace.""" <|
    \_ ->
    (E.object
      [ ( "boolean"
        , (E.bool True)
        )
      ])
    |> render26
    |> equal """ |  
  | 
"""



{- Indented Inline Sections
The template is:

---
 {{#boolean}}YES{{/boolean}}
 {{#boolean}}GOOD{{/boolean}}

---
The data is:

---
{
    "boolean": true
}
---
-}

render27 : Value -> String
render27 json = (\context0 ->
    """ """ ++ (section context0 ["boolean"] (\context1 ->"""YES""")) ++ """
 """ ++ (section context0 ["boolean"] (\context1 ->"""GOOD""")) ++ """
""") [json]

suite27 : Test
suite27 = test """Indented Inline Sections — Single-line sections should not alter surrounding whitespace.""" <|
    \_ ->
    (E.object
      [ ( "boolean"
        , (E.bool True)
        )
      ])
    |> render27
    |> equal """ YES
 GOOD
"""



{- Standalone Lines
The template is:

---
| This Is
{{#boolean}}
|
{{/boolean}}
| A Line

---
The data is:

---
{
    "boolean": true
}
---
-}

render28 : Value -> String
render28 json = (\context0 ->
    """| This Is
""" ++ (section context0 ["boolean"] (\context1 ->"""|
""")) ++ """| A Line
""") [json]

suite28 : Test
suite28 = test """Standalone Lines — Standalone lines should be removed from the template.""" <|
    \_ ->
    (E.object
      [ ( "boolean"
        , (E.bool True)
        )
      ])
    |> render28
    |> equal """| This Is
|
| A Line
"""



{- Indented Standalone Lines
The template is:

---
| This Is
  {{#boolean}}
|
  {{/boolean}}
| A Line

---
The data is:

---
{
    "boolean": true
}
---
-}

render29 : Value -> String
render29 json = (\context0 ->
    """| This Is
""" ++ (section context0 ["boolean"] (\context1 ->"""|
""")) ++ """| A Line
""") [json]

suite29 : Test
suite29 = test """Indented Standalone Lines — Indented standalone lines should be removed from the template.""" <|
    \_ ->
    (E.object
      [ ( "boolean"
        , (E.bool True)
        )
      ])
    |> render29
    |> equal """| This Is
|
| A Line
"""



{- Standalone Line Endings
The template is:

---
|
{{#boolean}}
{{/boolean}}
|
---
The data is:

---
{
    "boolean": true
}
---
-}

render30 : Value -> String
render30 json = (\context0 ->
    """|
""" ++ (section context0 ["boolean"] (\context1 ->"""""")) ++ """|""") [json]

suite30 : Test
suite30 = test """Standalone Line Endings — "\r\n" should be considered a newline for standalone tags.""" <|
    \_ ->
    (E.object
      [ ( "boolean"
        , (E.bool True)
        )
      ])
    |> render30
    |> equal """|
|"""



{- Standalone Without Previous Line
The template is:

---
  {{#boolean}}
#{{/boolean}}
/
---
The data is:

---
{
    "boolean": true
}
---
-}

render31 : Value -> String
render31 json = (\context0 ->
    (section context0 ["boolean"] (\context1 ->"""#""")) ++ """
/""") [json]

suite31 : Test
suite31 = test """Standalone Without Previous Line — Standalone tags should not require a newline to precede them.""" <|
    \_ ->
    (E.object
      [ ( "boolean"
        , (E.bool True)
        )
      ])
    |> render31
    |> equal """#
/"""



{- Standalone Without Newline
The template is:

---
#{{#boolean}}
/
  {{/boolean}}
---
The data is:

---
{
    "boolean": true
}
---
-}

render32 : Value -> String
render32 json = (\context0 ->
    """#""" ++ (section context0 ["boolean"] (\context1 ->"""
/
"""))) [json]

suite32 : Test
suite32 = test """Standalone Without Newline — Standalone tags should not require a newline to follow them.""" <|
    \_ ->
    (E.object
      [ ( "boolean"
        , (E.bool True)
        )
      ])
    |> render32
    |> equal """#
/
"""



{- Padding
The template is:

---
|{{# boolean }}={{/ boolean }}|
---
The data is:

---
{
    "boolean": true
}
---
-}

render33 : Value -> String
render33 json = (\context0 ->
    """|""" ++ (section context0 ["boolean"] (\context1 ->"""=""")) ++ """|""") [json]

suite33 : Test
suite33 = test """Padding — Superfluous in-tag whitespace should be ignored.""" <|
    \_ ->
    (E.object
      [ ( "boolean"
        , (E.bool True)
        )
      ])
    |> render33
    |> equal """|=|"""

