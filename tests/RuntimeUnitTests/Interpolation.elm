module RuntimeUnitTests.Interpolation exposing (..)

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
    """No Interpolation"""
    """Mustache-free templates should render as-is."""
    """Hello from {Mustache}!
"""
    (E.object [])
    """Hello from {Mustache}!
"""

suite1 : Test
suite1 = makeTest
    """Basic Interpolation"""
    """Unadorned tags should interpolate content into the template."""
    """Hello, {{subject}}!
"""
    (E.object
      [ ( "subject"
        , (E.string """world""")
        )
      ])
    """Hello, world!
"""

suite2 : Test
suite2 = makeTest
    """HTML Escaping"""
    """Basic interpolation should be HTML escaped."""
    """These characters should be HTML escaped: {{forbidden}}
"""
    (E.object
      [ ( "forbidden"
        , (E.string """& \" < >""")
        )
      ])
    """These characters should be HTML escaped: &amp; &quot; &lt; &gt;
"""

suite3 : Test
suite3 = makeTest
    """Triple Mustache"""
    """Triple mustaches should interpolate without HTML escaping."""
    """These characters should not be HTML escaped: {{{forbidden}}}
"""
    (E.object
      [ ( "forbidden"
        , (E.string """& \" < >""")
        )
      ])
    """These characters should not be HTML escaped: & \" < >
"""

suite4 : Test
suite4 = makeTest
    """Ampersand"""
    """Ampersand should interpolate without HTML escaping."""
    """These characters should not be HTML escaped: {{&forbidden}}
"""
    (E.object
      [ ( "forbidden"
        , (E.string """& \" < >""")
        )
      ])
    """These characters should not be HTML escaped: & \" < >
"""

suite5 : Test
suite5 = makeTest
    """Basic Integer Interpolation"""
    """Integers should interpolate seamlessly."""
    """\"{{mph}} miles an hour!\""""
    (E.object
      [ ( "mph"
        , (E.int 85)
        )
      ])
    """\"85 miles an hour!\""""

suite6 : Test
suite6 = makeTest
    """Triple Mustache Integer Interpolation"""
    """Integers should interpolate seamlessly."""
    """\"{{{mph}}} miles an hour!\""""
    (E.object
      [ ( "mph"
        , (E.int 85)
        )
      ])
    """\"85 miles an hour!\""""

suite7 : Test
suite7 = makeTest
    """Ampersand Integer Interpolation"""
    """Integers should interpolate seamlessly."""
    """\"{{&mph}} miles an hour!\""""
    (E.object
      [ ( "mph"
        , (E.int 85)
        )
      ])
    """\"85 miles an hour!\""""

suite8 : Test
suite8 = makeTest
    """Basic Decimal Interpolation"""
    """Decimals should interpolate seamlessly with proper significance."""
    """\"{{power}} jiggawatts!\""""
    (E.object
      [ ( "power"
        , (E.float 1.21)
        )
      ])
    """\"1.21 jiggawatts!\""""

suite9 : Test
suite9 = makeTest
    """Triple Mustache Decimal Interpolation"""
    """Decimals should interpolate seamlessly with proper significance."""
    """\"{{{power}}} jiggawatts!\""""
    (E.object
      [ ( "power"
        , (E.float 1.21)
        )
      ])
    """\"1.21 jiggawatts!\""""

suite10 : Test
suite10 = makeTest
    """Ampersand Decimal Interpolation"""
    """Decimals should interpolate seamlessly with proper significance."""
    """\"{{&power}} jiggawatts!\""""
    (E.object
      [ ( "power"
        , (E.float 1.21)
        )
      ])
    """\"1.21 jiggawatts!\""""

suite11 : Test
suite11 = makeTest
    """Basic Null Interpolation"""
    """Nulls should interpolate as the empty string."""
    """I ({{cannot}}) be seen!"""
    (E.object
      [ ( "cannot"
        , E.null
        )
      ])
    """I () be seen!"""

suite12 : Test
suite12 = makeTest
    """Triple Mustache Null Interpolation"""
    """Nulls should interpolate as the empty string."""
    """I ({{{cannot}}}) be seen!"""
    (E.object
      [ ( "cannot"
        , E.null
        )
      ])
    """I () be seen!"""

suite13 : Test
suite13 = makeTest
    """Ampersand Null Interpolation"""
    """Nulls should interpolate as the empty string."""
    """I ({{&cannot}}) be seen!"""
    (E.object
      [ ( "cannot"
        , E.null
        )
      ])
    """I () be seen!"""

suite14 : Test
suite14 = makeTest
    """Basic Context Miss Interpolation"""
    """Failed context lookups should default to empty strings."""
    """I ({{cannot}}) be seen!"""
    (E.object [])
    """I () be seen!"""

suite15 : Test
suite15 = makeTest
    """Triple Mustache Context Miss Interpolation"""
    """Failed context lookups should default to empty strings."""
    """I ({{{cannot}}}) be seen!"""
    (E.object [])
    """I () be seen!"""

suite16 : Test
suite16 = makeTest
    """Ampersand Context Miss Interpolation"""
    """Failed context lookups should default to empty strings."""
    """I ({{&cannot}}) be seen!"""
    (E.object [])
    """I () be seen!"""

suite17 : Test
suite17 = makeTest
    """Dotted Names - Basic Interpolation"""
    """Dotted names should be considered a form of shorthand for sections."""
    """\"{{person.name}}\" == \"{{#person}}{{name}}{{/person}}\""""
    (E.object
      [ ( "person"
        , (E.object
          [ ( "name"
            , (E.string """Joe""")
            )
          ])
        )
      ])
    """\"Joe\" == \"Joe\""""

suite18 : Test
suite18 = makeTest
    """Dotted Names - Triple Mustache Interpolation"""
    """Dotted names should be considered a form of shorthand for sections."""
    """\"{{{person.name}}}\" == \"{{#person}}{{{name}}}{{/person}}\""""
    (E.object
      [ ( "person"
        , (E.object
          [ ( "name"
            , (E.string """Joe""")
            )
          ])
        )
      ])
    """\"Joe\" == \"Joe\""""

suite19 : Test
suite19 = makeTest
    """Dotted Names - Ampersand Interpolation"""
    """Dotted names should be considered a form of shorthand for sections."""
    """\"{{&person.name}}\" == \"{{#person}}{{&name}}{{/person}}\""""
    (E.object
      [ ( "person"
        , (E.object
          [ ( "name"
            , (E.string """Joe""")
            )
          ])
        )
      ])
    """\"Joe\" == \"Joe\""""

suite20 : Test
suite20 = makeTest
    """Dotted Names - Arbitrary Depth"""
    """Dotted names should be functional to any level of nesting."""
    """\"{{a.b.c.d.e.name}}\" == \"Phil\""""
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
    """\"Phil\" == \"Phil\""""

suite21 : Test
suite21 = makeTest
    """Dotted Names - Broken Chains"""
    """Any falsey value prior to the last part of the name should yield ''."""
    """\"{{a.b.c}}\" == \"\""""
    (E.object
      [ ( "a"
        , (E.object [])
        )
      ])
    """\"\" == \"\""""

suite22 : Test
suite22 = makeTest
    """Dotted Names - Broken Chain Resolution"""
    """Each part of a dotted name should resolve only against its parent."""
    """\"{{a.b.c.name}}\" == \"\""""
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
    """\"\" == \"\""""

suite23 : Test
suite23 = makeTest
    """Dotted Names - Initial Resolution"""
    """The first part of a dotted name should resolve as any other name."""
    """\"{{#a}}{{b.c.d.e.name}}{{/a}}\" == \"Phil\""""
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
    """\"Phil\" == \"Phil\""""

suite24 : Test
suite24 = makeTest
    """Dotted Names - Context Precedence"""
    """Dotted names should be resolved against former resolutions."""
    """{{#a}}{{b.c}}{{/a}}"""
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
    """"""

suite25 : Test
suite25 = makeTest
    """Implicit Iterators - Basic Interpolation"""
    """Unadorned tags should interpolate content into the template."""
    """Hello, {{.}}!
"""
    (E.string """world""")
    """Hello, world!
"""

suite26 : Test
suite26 = makeTest
    """Implicit Iterators - HTML Escaping"""
    """Basic interpolation should be HTML escaped."""
    """These characters should be HTML escaped: {{.}}
"""
    (E.string """& \" < >""")
    """These characters should be HTML escaped: &amp; &quot; &lt; &gt;
"""

suite27 : Test
suite27 = makeTest
    """Implicit Iterators - Triple Mustache"""
    """Triple mustaches should interpolate without HTML escaping."""
    """These characters should not be HTML escaped: {{{.}}}
"""
    (E.string """& \" < >""")
    """These characters should not be HTML escaped: & \" < >
"""

suite28 : Test
suite28 = makeTest
    """Implicit Iterators - Ampersand"""
    """Ampersand should interpolate without HTML escaping."""
    """These characters should not be HTML escaped: {{&.}}
"""
    (E.string """& \" < >""")
    """These characters should not be HTML escaped: & \" < >
"""

suite29 : Test
suite29 = makeTest
    """Implicit Iterators - Basic Integer Interpolation"""
    """Integers should interpolate seamlessly."""
    """\"{{.}} miles an hour!\""""
    (E.int 85)
    """\"85 miles an hour!\""""

suite30 : Test
suite30 = makeTest
    """Interpolation - Surrounding Whitespace"""
    """Interpolation should not alter surrounding whitespace."""
    """| {{string}} |"""
    (E.object
      [ ( "string"
        , (E.string """---""")
        )
      ])
    """| --- |"""

suite31 : Test
suite31 = makeTest
    """Triple Mustache - Surrounding Whitespace"""
    """Interpolation should not alter surrounding whitespace."""
    """| {{{string}}} |"""
    (E.object
      [ ( "string"
        , (E.string """---""")
        )
      ])
    """| --- |"""

suite32 : Test
suite32 = makeTest
    """Ampersand - Surrounding Whitespace"""
    """Interpolation should not alter surrounding whitespace."""
    """| {{&string}} |"""
    (E.object
      [ ( "string"
        , (E.string """---""")
        )
      ])
    """| --- |"""

suite33 : Test
suite33 = makeTest
    """Interpolation - Standalone"""
    """Standalone interpolation should not alter surrounding whitespace."""
    """  {{string}}
"""
    (E.object
      [ ( "string"
        , (E.string """---""")
        )
      ])
    """  ---
"""

suite34 : Test
suite34 = makeTest
    """Triple Mustache - Standalone"""
    """Standalone interpolation should not alter surrounding whitespace."""
    """  {{{string}}}
"""
    (E.object
      [ ( "string"
        , (E.string """---""")
        )
      ])
    """  ---
"""

suite35 : Test
suite35 = makeTest
    """Ampersand - Standalone"""
    """Standalone interpolation should not alter surrounding whitespace."""
    """  {{&string}}
"""
    (E.object
      [ ( "string"
        , (E.string """---""")
        )
      ])
    """  ---
"""

suite36 : Test
suite36 = makeTest
    """Interpolation With Padding"""
    """Superfluous in-tag whitespace should be ignored."""
    """|{{ string }}|"""
    (E.object
      [ ( "string"
        , (E.string """---""")
        )
      ])
    """|---|"""

suite37 : Test
suite37 = makeTest
    """Triple Mustache With Padding"""
    """Superfluous in-tag whitespace should be ignored."""
    """|{{{ string }}}|"""
    (E.object
      [ ( "string"
        , (E.string """---""")
        )
      ])
    """|---|"""

suite38 : Test
suite38 = makeTest
    """Ampersand With Padding"""
    """Superfluous in-tag whitespace should be ignored."""
    """|{{& string }}|"""
    (E.object
      [ ( "string"
        , (E.string """---""")
        )
      ])
    """|---|"""

