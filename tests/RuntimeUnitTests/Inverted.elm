module RuntimeUnitTests.Inverted exposing (..)

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
    """Falsey"""
    """Falsey sections should have their contents rendered."""
    """\"{{^boolean}}This should be rendered.{{/boolean}}\""""
    (E.object
      [ ( "boolean"
        , (E.bool False)
        )
      ])
    """\"This should be rendered.\""""

suite1 : Test
suite1 = makeTest
    """Truthy"""
    """Truthy sections should have their contents omitted."""
    """\"{{^boolean}}This should not be rendered.{{/boolean}}\""""
    (E.object
      [ ( "boolean"
        , (E.bool True)
        )
      ])
    """\"\""""

suite2 : Test
suite2 = makeTest
    """Null is falsey"""
    """Null is falsey."""
    """\"{{^null}}This should be rendered.{{/null}}\""""
    (E.object
      [ ( "null"
        , E.null
        )
      ])
    """\"This should be rendered.\""""

suite3 : Test
suite3 = makeTest
    """Context"""
    """Objects and hashes should behave like truthy values."""
    """\"{{^context}}Hi {{name}}.{{/context}}\""""
    (E.object
      [ ( "context"
        , (E.object
          [ ( "name"
            , (E.string """Joe""")
            )
          ])
        )
      ])
    """\"\""""

suite4 : Test
suite4 = makeTest
    """List"""
    """Lists should behave like truthy values."""
    """\"{{^list}}{{n}}{{/list}}\""""
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
    """\"\""""

suite5 : Test
suite5 = makeTest
    """Empty List"""
    """Empty lists should behave like falsey values."""
    """\"{{^list}}Yay lists!{{/list}}\""""
    (E.object
      [ ( "list"
        , (E.list identity [])
        )
      ])
    """\"Yay lists!\""""

suite6 : Test
suite6 = makeTest
    """Doubled"""
    """Multiple inverted sections per template should be permitted."""
    """{{^bool}}
* first
{{/bool}}
* {{two}}
{{^bool}}
* third
{{/bool}}
"""
    (E.object
      [ ( "bool"
        , (E.bool False)
        )
      , ( "two"
        , (E.string """second""")
        )
      ])
    """* first
* second
* third
"""

suite7 : Test
suite7 = makeTest
    """Nested (Falsey)"""
    """Nested falsey sections should have their contents rendered."""
    """| A {{^bool}}B {{^bool}}C{{/bool}} D{{/bool}} E |"""
    (E.object
      [ ( "bool"
        , (E.bool False)
        )
      ])
    """| A B C D E |"""

suite8 : Test
suite8 = makeTest
    """Nested (Truthy)"""
    """Nested truthy sections should be omitted."""
    """| A {{^bool}}B {{^bool}}C{{/bool}} D{{/bool}} E |"""
    (E.object
      [ ( "bool"
        , (E.bool True)
        )
      ])
    """| A  E |"""

suite9 : Test
suite9 = makeTest
    """Context Misses"""
    """Failed context lookups should be considered falsey."""
    """[{{^missing}}Cannot find key 'missing'!{{/missing}}]"""
    (E.object [])
    """[Cannot find key 'missing'!]"""

suite10 : Test
suite10 = makeTest
    """Dotted Names - Truthy"""
    """Dotted names should be valid for Inverted Section tags."""
    """\"{{^a.b.c}}Not Here{{/a.b.c}}\" == \"\""""
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
    """\"\" == \"\""""

suite11 : Test
suite11 = makeTest
    """Dotted Names - Falsey"""
    """Dotted names should be valid for Inverted Section tags."""
    """\"{{^a.b.c}}Not Here{{/a.b.c}}\" == \"Not Here\""""
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
    """\"Not Here\" == \"Not Here\""""

suite12 : Test
suite12 = makeTest
    """Dotted Names - Broken Chains"""
    """Dotted names that cannot be resolved should be considered falsey."""
    """\"{{^a.b.c}}Not Here{{/a.b.c}}\" == \"Not Here\""""
    (E.object
      [ ( "a"
        , (E.object [])
        )
      ])
    """\"Not Here\" == \"Not Here\""""

suite13 : Test
suite13 = makeTest
    """Surrounding Whitespace"""
    """Inverted sections should not alter surrounding whitespace."""
    """ | {{^boolean}}	|	{{/boolean}} | 
"""
    (E.object
      [ ( "boolean"
        , (E.bool False)
        )
      ])
    """ | 	|	 | 
"""

suite14 : Test
suite14 = makeTest
    """Internal Whitespace"""
    """Inverted should not alter internal whitespace."""
    """ | {{^boolean}} {{! Important Whitespace }}
 {{/boolean}} | 
"""
    (E.object
      [ ( "boolean"
        , (E.bool False)
        )
      ])
    """ |  
  | 
"""

suite15 : Test
suite15 = makeTest
    """Indented Inline Sections"""
    """Single-line sections should not alter surrounding whitespace."""
    """ {{^boolean}}NO{{/boolean}}
 {{^boolean}}WAY{{/boolean}}
"""
    (E.object
      [ ( "boolean"
        , (E.bool False)
        )
      ])
    """ NO
 WAY
"""

suite16 : Test
suite16 = makeTest
    """Standalone Lines"""
    """Standalone lines should be removed from the template."""
    """| This Is
{{^boolean}}
|
{{/boolean}}
| A Line
"""
    (E.object
      [ ( "boolean"
        , (E.bool False)
        )
      ])
    """| This Is
|
| A Line
"""

suite17 : Test
suite17 = makeTest
    """Standalone Indented Lines"""
    """Standalone indented lines should be removed from the template."""
    """| This Is
  {{^boolean}}
|
  {{/boolean}}
| A Line
"""
    (E.object
      [ ( "boolean"
        , (E.bool False)
        )
      ])
    """| This Is
|
| A Line
"""

suite18 : Test
suite18 = makeTest
    """Standalone Without Previous Line"""
    """Standalone tags should not require a newline to precede them."""
    """  {{^boolean}}
^{{/boolean}}
/"""
    (E.object
      [ ( "boolean"
        , (E.bool False)
        )
      ])
    """^
/"""

suite19 : Test
suite19 = makeTest
    """Standalone Without Newline"""
    """Standalone tags should not require a newline to follow them."""
    """^{{^boolean}}
/
  {{/boolean}}"""
    (E.object
      [ ( "boolean"
        , (E.bool False)
        )
      ])
    """^
/
"""

suite20 : Test
suite20 = makeTest
    """Padding"""
    """Superfluous in-tag whitespace should be ignored."""
    """|{{^ boolean }}={{/ boolean }}|"""
    (E.object
      [ ( "boolean"
        , (E.bool False)
        )
      ])
    """|=|"""

