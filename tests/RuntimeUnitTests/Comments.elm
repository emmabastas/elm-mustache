module RuntimeUnitTests.Comments exposing (..)

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
    """Inline"""
    """Comment blocks should be removed from the template."""
    """12345{{! Comment Block! }}67890"""
    (E.object [])
    """1234567890"""

suite1 : Test
suite1 = makeTest
    """Multiline"""
    """Multiline comments should be permitted."""
    """12345{{!
  This is a
  multi-line comment...
}}67890
"""
    (E.object [])
    """1234567890
"""

suite2 : Test
suite2 = makeTest
    """Standalone"""
    """All standalone comment lines should be removed."""
    """Begin.
{{! Comment Block! }}
End.
"""
    (E.object [])
    """Begin.
End.
"""

suite3 : Test
suite3 = makeTest
    """Indented Standalone"""
    """All standalone comment lines should be removed."""
    """Begin.
  {{! Indented Comment Block! }}
End.
"""
    (E.object [])
    """Begin.
End.
"""

suite4 : Test
suite4 = makeTest
    """Standalone Without Previous Line"""
    """Standalone tags should not require a newline to precede them."""
    """  {{! I'm Still Standalone }}
!"""
    (E.object [])
    """!"""

suite5 : Test
suite5 = makeTest
    """Standalone Without Newline"""
    """Standalone tags should not require a newline to follow them."""
    """!
  {{! I'm Still Standalone }}"""
    (E.object [])
    """!
"""

suite6 : Test
suite6 = makeTest
    """Multiline Standalone"""
    """All standalone comment lines should be removed."""
    """Begin.
{{!
Something's going on here...
}}
End.
"""
    (E.object [])
    """Begin.
End.
"""

suite7 : Test
suite7 = makeTest
    """Indented Multiline Standalone"""
    """All standalone comment lines should be removed."""
    """Begin.
  {{!
    Something's going on here...
  }}
End.
"""
    (E.object [])
    """Begin.
End.
"""

suite8 : Test
suite8 = makeTest
    """Indented Inline"""
    """Inline comments should not strip whitespace"""
    """  12 {{! 34 }}
"""
    (E.object [])
    """  12 
"""

suite9 : Test
suite9 = makeTest
    """Surrounding Whitespace"""
    """Comment removal should preserve surrounding whitespace."""
    """12345 {{! Comment Block! }} 67890"""
    (E.object [])
    """12345  67890"""

suite10 : Test
suite10 = makeTest
    """Variable Name Collision"""
    """Comments must never render, even if variable with same name exists."""
    """comments never show: >{{! comment }}<"""
    (E.object
      [ ( "! comment"
        , (E.int 1)
        )
      , ( "! comment "
        , (E.int 2)
        )
      , ( "!comment"
        , (E.int 3)
        )
      , ( "comment"
        , (E.int 4)
        )
      ])
    """comments never show: ><"""

