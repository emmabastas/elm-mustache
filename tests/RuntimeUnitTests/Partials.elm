module RuntimeUnitTests.Partials exposing (..)

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
    """Basic Behavior"""
    """The greater-than operator should expand to the named partial."""
    """\"{{>text}}\""""
    (E.object
      [ ( "text"
        , (E.string """from partial""")
        )
      ])
    """\"from partial\""""

suite1 : Test
suite1 = makeTest
    """Failed Lookup"""
    """The empty string should be used when the named partial is not found."""
    """\"{{>text}}\""""
    (E.object [])
    """\"\""""

suite2 : Test
suite2 = makeTest
    """Context"""
    """The greater-than operator should operate within the current context."""
    """\"{{>partial}}\""""
    (E.object
      [ ( "text"
        , (E.string """content""")
        )
      , ( "partial"
        , (E.string """*{{text}}*""")
        )
      ])
    """\"*content*\""""

suite3 : Test
suite3 = makeTest
    """Recursion"""
    """The greater-than operator should properly recurse."""
    """{{>node}}"""
    (E.object
      [ ( "content"
        , (E.string """X""")
        )
      , ( "nodes"
        , (E.list identity [(E.object
            [ ( "content"
              , (E.string """Y""")
              )
            , ( "nodes"
              , (E.list identity [])
              )
            ])])
        )
      , ( "node"
        , (E.string """{{content}}<{{#nodes}}{{>node}}{{/nodes}}>""")
        )
      ])
    """X<Y<>>"""

suite4 : Test
suite4 = makeTest
    """Nested"""
    """The greater-than operator should work from within partials."""
    """{{>outer}}"""
    (E.object
      [ ( "a"
        , (E.string """hello""")
        )
      , ( "b"
        , (E.string """world""")
        )
      , ( "outer"
        , (E.string """*{{a}} {{>inner}}*""")
        )
      , ( "inner"
        , (E.string """{{b}}!""")
        )
      ])
    """*hello world!*"""

suite5 : Test
suite5 = makeTest
    """Surrounding Whitespace"""
    """The greater-than operator should not alter surrounding whitespace."""
    """| {{>partial}} |"""
    (E.object
      [ ( "partial"
        , (E.string """	|	""")
        )
      ])
    """| 	|	 |"""

suite6 : Test
suite6 = makeTest
    """Inline Indentation"""
    """Whitespace should be left untouched."""
    """  {{data}}  {{> partial}}
"""
    (E.object
      [ ( "data"
        , (E.string """|""")
        )
      , ( "partial"
        , (E.string """>
>""")
        )
      ])
    """  |  >
>
"""

suite7 : Test
suite7 = makeTest
    """Standalone Without Previous Line"""
    """Standalone tags should not require a newline to precede them."""
    """  {{>partial}}
>"""
    (E.object
      [ ( "partial"
        , (E.string """>
>""")
        )
      ])
    """  >
  >>"""

suite8 : Test
suite8 = makeTest
    """Standalone Without Newline"""
    """Standalone tags should not require a newline to follow them."""
    """>
  {{>partial}}"""
    (E.object
      [ ( "partial"
        , (E.string """>
>""")
        )
      ])
    """>
  >
  >"""

suite9 : Test
suite9 = makeTest
    """Standalone Indentation"""
    """Each line of the partial should be indented before rendering."""
    """\\
 {{>partial}}
/
"""
    (E.object
      [ ( "content"
        , (E.string """<
->""")
        )
      , ( "partial"
        , (E.string """|
{{{content}}}
|
""")
        )
      ])
    """\\
 |
 <
->
 |
/
"""

suite10 : Test
suite10 = makeTest
    """Padding Whitespace"""
    """Superfluous in-tag whitespace should be ignored."""
    """|{{> partial }}|"""
    (E.object
      [ ( "boolean"
        , (E.bool True)
        )
      , ( "partial"
        , (E.string """[]""")
        )
      ])
    """|[]|"""

