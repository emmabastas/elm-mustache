module RuntimeUnitTests.Sections exposing (..)

import Mustache exposing (render)
import Test exposing (Test, test)
import Expect exposing (equal)
import Json.Decode exposing (Value)
import Json.Encode as E

makeTest : String -> String -> String -> Value -> String -> Test
makeTest name desc template data expected =
    test (name ++ " — " ++ desc) <|
        \_ ->
        data
        |> render template
        |> equal (Ok expected)

suite0 : Test
suite0 = makeTest
    """Truthy"""
    """Truthy sections should have their contents rendered."""
    """\"{{#boolean}}This should be rendered.{{/boolean}}\""""
    (E.object
      [ ( "boolean"
        , (E.bool True)
        )
      ])
    """\"This should be rendered.\""""

suite1 : Test
suite1 = makeTest
    """Falsey"""
    """Falsey sections should have their contents omitted."""
    """\"{{#boolean}}This should not be rendered.{{/boolean}}\""""
    (E.object
      [ ( "boolean"
        , (E.bool False)
        )
      ])
    """\"\""""

suite2 : Test
suite2 = makeTest
    """Null is falsey"""
    """Null is falsey."""
    """\"{{#null}}This should not be rendered.{{/null}}\""""
    (E.object
      [ ( "null"
        , E.null
        )
      ])
    """\"\""""

suite3 : Test
suite3 = makeTest
    """Context"""
    """Objects and hashes should be pushed onto the context stack."""
    """\"{{#context}}Hi {{name}}.{{/context}}\""""
    (E.object
      [ ( "context"
        , (E.object
          [ ( "name"
            , (E.string """Joe""")
            )
          ])
        )
      ])
    """\"Hi Joe.\""""

suite4 : Test
suite4 = makeTest
    """Parent contexts"""
    """Names missing in the current context are looked up in the stack."""
    """\"{{#sec}}{{a}}, {{b}}, {{c.d}}{{/sec}}\""""
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
    """\"foo, bar, baz\""""

suite5 : Test
suite5 = makeTest
    """Variable test"""
    """Non-false sections have their value at the top of context,
accessible as {{.}} or through the parent context. This gives
a simple way to display content conditionally if a variable exists.
"""
    """\"{{#foo}}{{.}} is {{foo}}{{/foo}}\""""
    (E.object
      [ ( "foo"
        , (E.string """bar""")
        )
      ])
    """\"bar is bar\""""

suite6 : Test
suite6 = makeTest
    """List Contexts"""
    """All elements on the context stack should be accessible within lists."""
    """{{#tops}}{{#middles}}{{tname.lower}}{{mname}}.{{#bottoms}}{{tname.upper}}{{mname}}{{bname}}.{{/bottoms}}{{/middles}}{{/tops}}"""
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
    """a1.A1x.A1y."""

suite7 : Test
suite7 = makeTest
    """Deeply Nested Contexts"""
    """All elements on the context stack should be accessible."""
    """{{#a}}
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
"""
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
    """1
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

suite8 : Test
suite8 = makeTest
    """List"""
    """Lists should be iterated; list items should visit the context stack."""
    """\"{{#list}}{{item}}{{/list}}\""""
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
    """\"123\""""

suite9 : Test
suite9 = makeTest
    """Empty List"""
    """Empty lists should behave like falsey values."""
    """\"{{#list}}Yay lists!{{/list}}\""""
    (E.object
      [ ( "list"
        , (E.list identity [])
        )
      ])
    """\"\""""

suite10 : Test
suite10 = makeTest
    """Doubled"""
    """Multiple sections per template should be permitted."""
    """{{#bool}}
* first
{{/bool}}
* {{two}}
{{#bool}}
* third
{{/bool}}
"""
    (E.object
      [ ( "bool"
        , (E.bool True)
        )
      , ( "two"
        , (E.string """second""")
        )
      ])
    """* first
* second
* third
"""

suite11 : Test
suite11 = makeTest
    """Nested (Truthy)"""
    """Nested truthy sections should have their contents rendered."""
    """| A {{#bool}}B {{#bool}}C{{/bool}} D{{/bool}} E |"""
    (E.object
      [ ( "bool"
        , (E.bool True)
        )
      ])
    """| A B C D E |"""

suite12 : Test
suite12 = makeTest
    """Nested (Falsey)"""
    """Nested falsey sections should be omitted."""
    """| A {{#bool}}B {{#bool}}C{{/bool}} D{{/bool}} E |"""
    (E.object
      [ ( "bool"
        , (E.bool False)
        )
      ])
    """| A  E |"""

suite13 : Test
suite13 = makeTest
    """Context Misses"""
    """Failed context lookups should be considered falsey."""
    """[{{#missing}}Found key 'missing'!{{/missing}}]"""
    (E.object [])
    """[]"""

suite14 : Test
suite14 = makeTest
    """Implicit Iterator - String"""
    """Implicit iterators should directly interpolate strings."""
    """\"{{#list}}({{.}}){{/list}}\""""
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
    """\"(a)(b)(c)(d)(e)\""""

suite15 : Test
suite15 = makeTest
    """Implicit Iterator - Integer"""
    """Implicit iterators should cast integers to strings and interpolate."""
    """\"{{#list}}({{.}}){{/list}}\""""
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
    """\"(1)(2)(3)(4)(5)\""""

suite16 : Test
suite16 = makeTest
    """Implicit Iterator - Decimal"""
    """Implicit iterators should cast decimals to strings and interpolate."""
    """\"{{#list}}({{.}}){{/list}}\""""
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
    """\"(1.1)(2.2)(3.3)(4.4)(5.5)\""""

suite17 : Test
suite17 = makeTest
    """Implicit Iterator - Array"""
    """Implicit iterators should allow iterating over nested arrays."""
    """\"{{#list}}({{#.}}{{.}}{{/.}}){{/list}}\""""
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
    """\"(123)(abc)\""""

suite18 : Test
suite18 = makeTest
    """Implicit Iterator - HTML Escaping"""
    """Implicit iterators with basic interpolation should be HTML escaped."""
    """\"{{#list}}({{.}}){{/list}}\""""
    (E.object
      [ ( "list"
        , (E.list identity [ (E.string """&""")
        , (E.string """\"""")
        , (E.string """<""")
        , (E.string """>""")
        ])
        )
      ])
    """\"(&amp;)(&quot;)(&lt;)(&gt;)\""""

suite19 : Test
suite19 = makeTest
    """Implicit Iterator - Triple mustache"""
    """Implicit iterators in triple mustache should interpolate without HTML escaping."""
    """\"{{#list}}({{{.}}}){{/list}}\""""
    (E.object
      [ ( "list"
        , (E.list identity [ (E.string """&""")
        , (E.string """\"""")
        , (E.string """<""")
        , (E.string """>""")
        ])
        )
      ])
    """\"(&)(\")(<)(>)\""""

suite20 : Test
suite20 = makeTest
    """Implicit Iterator - Ampersand"""
    """Implicit iterators in an Ampersand tag should interpolate without HTML escaping."""
    """\"{{#list}}({{&.}}){{/list}}\""""
    (E.object
      [ ( "list"
        , (E.list identity [ (E.string """&""")
        , (E.string """\"""")
        , (E.string """<""")
        , (E.string """>""")
        ])
        )
      ])
    """\"(&)(\")(<)(>)\""""

suite21 : Test
suite21 = makeTest
    """Implicit Iterator - Root-level"""
    """Implicit iterators should work on root-level lists."""
    """\"{{#.}}({{value}}){{/.}}\""""
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
    """\"(a)(b)\""""

suite22 : Test
suite22 = makeTest
    """Dotted Names - Truthy"""
    """Dotted names should be valid for Section tags."""
    """\"{{#a.b.c}}Here{{/a.b.c}}\" == \"Here\""""
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
    """\"Here\" == \"Here\""""

suite23 : Test
suite23 = makeTest
    """Dotted Names - Falsey"""
    """Dotted names should be valid for Section tags."""
    """\"{{#a.b.c}}Here{{/a.b.c}}\" == \"\""""
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
    """\"\" == \"\""""

suite24 : Test
suite24 = makeTest
    """Dotted Names - Broken Chains"""
    """Dotted names that cannot be resolved should be considered falsey."""
    """\"{{#a.b.c}}Here{{/a.b.c}}\" == \"\""""
    (E.object
      [ ( "a"
        , (E.object [])
        )
      ])
    """\"\" == \"\""""

suite25 : Test
suite25 = makeTest
    """Surrounding Whitespace"""
    """Sections should not alter surrounding whitespace."""
    """ | {{#boolean}}	|	{{/boolean}} | 
"""
    (E.object
      [ ( "boolean"
        , (E.bool True)
        )
      ])
    """ | 	|	 | 
"""

suite26 : Test
suite26 = makeTest
    """Internal Whitespace"""
    """Sections should not alter internal whitespace."""
    """ | {{#boolean}} {{! Important Whitespace }}
 {{/boolean}} | 
"""
    (E.object
      [ ( "boolean"
        , (E.bool True)
        )
      ])
    """ |  
  | 
"""

suite27 : Test
suite27 = makeTest
    """Indented Inline Sections"""
    """Single-line sections should not alter surrounding whitespace."""
    """ {{#boolean}}YES{{/boolean}}
 {{#boolean}}GOOD{{/boolean}}
"""
    (E.object
      [ ( "boolean"
        , (E.bool True)
        )
      ])
    """ YES
 GOOD
"""

suite28 : Test
suite28 = makeTest
    """Standalone Lines"""
    """Standalone lines should be removed from the template."""
    """| This Is
{{#boolean}}
|
{{/boolean}}
| A Line
"""
    (E.object
      [ ( "boolean"
        , (E.bool True)
        )
      ])
    """| This Is
|
| A Line
"""

suite29 : Test
suite29 = makeTest
    """Indented Standalone Lines"""
    """Indented standalone lines should be removed from the template."""
    """| This Is
  {{#boolean}}
|
  {{/boolean}}
| A Line
"""
    (E.object
      [ ( "boolean"
        , (E.bool True)
        )
      ])
    """| This Is
|
| A Line
"""

suite30 : Test
suite30 = makeTest
    """Standalone Without Previous Line"""
    """Standalone tags should not require a newline to precede them."""
    """  {{#boolean}}
#{{/boolean}}
/"""
    (E.object
      [ ( "boolean"
        , (E.bool True)
        )
      ])
    """#
/"""

suite31 : Test
suite31 = makeTest
    """Standalone Without Newline"""
    """Standalone tags should not require a newline to follow them."""
    """#{{#boolean}}
/
  {{/boolean}}"""
    (E.object
      [ ( "boolean"
        , (E.bool True)
        )
      ])
    """#
/
"""

suite32 : Test
suite32 = makeTest
    """Padding"""
    """Superfluous in-tag whitespace should be ignored."""
    """|{{# boolean }}={{/ boolean }}|"""
    (E.object
      [ ( "boolean"
        , (E.bool True)
        )
      ])
    """|=|"""

