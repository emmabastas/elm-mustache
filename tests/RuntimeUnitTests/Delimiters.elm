module RuntimeUnitTests.Delimiters exposing (..)

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
    """Pair Behavior"""
    """The equals sign (used on both sides) should permit delimiter changes."""
    """{{=<% %>=}}(<%text%>)"""
    (E.object
      [ ( "text"
        , (E.string """Hey!""")
        )
      ])
    """(Hey!)"""

suite1 : Test
suite1 = makeTest
    """Special Characters"""
    """Characters with special meaning regexen should be valid delimiters."""
    """({{=[ ]=}}[text])"""
    (E.object
      [ ( "text"
        , (E.string """It worked!""")
        )
      ])
    """(It worked!)"""

suite2 : Test
suite2 = makeTest
    """Sections"""
    """Delimiters set outside sections should persist."""
    """[
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
"""
    (E.object
      [ ( "section"
        , (E.bool True)
        )
      , ( "data"
        , (E.string """I got interpolated.""")
        )
      ])
    """[
  I got interpolated.
  |data|

  {{data}}
  I got interpolated.
]
"""

suite3 : Test
suite3 = makeTest
    """Inverted Sections"""
    """Delimiters set outside inverted sections should persist."""
    """[
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
"""
    (E.object
      [ ( "section"
        , (E.bool False)
        )
      , ( "data"
        , (E.string """I got interpolated.""")
        )
      ])
    """[
  I got interpolated.
  |data|

  {{data}}
  I got interpolated.
]
"""

suite4 : Test
suite4 = makeTest
    """Surrounding Whitespace"""
    """Surrounding whitespace should be left untouched."""
    """| {{=@ @=}} |"""
    (E.object [])
    """|  |"""

suite5 : Test
suite5 = makeTest
    """Outlying Whitespace (Inline)"""
    """Whitespace should be left untouched."""
    """ | {{=@ @=}}
"""
    (E.object [])
    """ | 
"""

suite6 : Test
suite6 = makeTest
    """Standalone Tag"""
    """Standalone lines should be removed from the template."""
    """Begin.
{{=@ @=}}
End.
"""
    (E.object [])
    """Begin.
End.
"""

suite7 : Test
suite7 = makeTest
    """Indented Standalone Tag"""
    """Indented standalone lines should be removed from the template."""
    """Begin.
  {{=@ @=}}
End.
"""
    (E.object [])
    """Begin.
End.
"""

suite8 : Test
suite8 = makeTest
    """Standalone Without Previous Line"""
    """Standalone tags should not require a newline to precede them."""
    """  {{=@ @=}}
="""
    (E.object [])
    """="""

suite9 : Test
suite9 = makeTest
    """Standalone Without Newline"""
    """Standalone tags should not require a newline to follow them."""
    """=
  {{=@ @=}}"""
    (E.object [])
    """=
"""

suite10 : Test
suite10 = makeTest
    """Pair with Padding"""
    """Superfluous in-tag whitespace should be ignored."""
    """|{{= @   @ =}}|"""
    (E.object [])
    """||"""

