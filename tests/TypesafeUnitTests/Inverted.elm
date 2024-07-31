module TypesafeUnitTests.Inverted exposing (..)

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

render0 : Context0 a -> String
render0 c =
    """\"""" ++ (if not c.boolean then ("""This should be rendered.""") else "") ++ """\""""

type alias Context0 a =
    { a
        | boolean : Bool
    }

suite0 : Test
suite0 = test """Falsey — Falsey sections should have their contents rendered.""" <|
    \_ ->
    { boolean =
        False
    }
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

render1 : Context1 a -> String
render1 c =
    """\"""" ++ (if not c.boolean then ("""This should not be rendered.""") else "") ++ """\""""

type alias Context1 a =
    { a
        | boolean : Bool
    }

suite1 : Test
suite1 = test """Truthy — Truthy sections should have their contents omitted.""" <|
    \_ ->
    { boolean =
        True
    }
    |> render1
    |> equal """\"\""""



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

render2 : Context2 a -> String
render2 c =
    (if not c.bool then ("""* first
""") else "") ++ """* """ ++ htmlEscape c.two ++ """
""" ++ (if not c.bool then ("""* third
""") else "")

type alias Context2 a =
    { a
        | bool : Bool
        , two : String
    }

suite2 : Test
suite2 = test """Doubled — Multiple inverted sections per template should be permitted.""" <|
    \_ ->
    { bool =
        False
    , two =
        """second"""
    }
    |> render2
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

render3 : Context3 a -> String
render3 c =
    """| A """ ++ (if not c.bool then ("""B """ ++ (if not c.bool then ("""C""") else "") ++ """ D""") else "") ++ """ E |"""

type alias Context3 a =
    { a
        | bool : Bool
    }

suite3 : Test
suite3 = test """Nested (Falsey) — Nested falsey sections should have their contents rendered.""" <|
    \_ ->
    { bool =
        False
    }
    |> render3
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

render4 : Context4 a -> String
render4 c =
    """| A """ ++ (if not c.bool then ("""B """ ++ (if not c.bool then ("""C""") else "") ++ """ D""") else "") ++ """ E |"""

type alias Context4 a =
    { a
        | bool : Bool
    }

suite4 : Test
suite4 = test """Nested (Truthy) — Nested truthy sections should be omitted.""" <|
    \_ ->
    { bool =
        True
    }
    |> render4
    |> equal """| A  E |"""



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

render5 : Context5 a -> String
render5 c =
    """\"""" ++ (if not c.a_b_c then ("""Not Here""") else "") ++ """\" == \"\""""

type alias Context5 a =
    { a
        | a_b_c : Bool
    }

suite5 : Test
suite5 = test """Dotted Names - Truthy — Dotted names should be valid for Inverted Section tags.""" <|
    \_ ->
    { a_b_c =
        True
    }
    |> render5
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

render6 : Context6 a -> String
render6 c =
    """\"""" ++ (if not c.a_b_c then ("""Not Here""") else "") ++ """\" == \"Not Here\""""

type alias Context6 a =
    { a
        | a_b_c : Bool
    }

suite6 : Test
suite6 = test """Dotted Names - Falsey — Dotted names should be valid for Inverted Section tags.""" <|
    \_ ->
    { a_b_c =
        False
    }
    |> render6
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

render7 : Context7 a -> String
render7 c =
    """ | """ ++ (if not c.boolean then ("""	|	""") else "") ++ """ | 
"""

type alias Context7 a =
    { a
        | boolean : Bool
    }

suite7 : Test
suite7 = test """Surrounding Whitespace — Inverted sections should not alter surrounding whitespace.""" <|
    \_ ->
    { boolean =
        False
    }
    |> render7
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

render8 : Context8 a -> String
render8 c =
    """ | """ ++ (if not c.boolean then (""" """ ++ comment ++ """
 """) else "") ++ """ | 
"""

type alias Context8 a =
    { a
        | boolean : Bool
    }

suite8 : Test
suite8 = test """Internal Whitespace — Inverted should not alter internal whitespace.""" <|
    \_ ->
    { boolean =
        False
    }
    |> render8
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

render9 : Context9 a -> String
render9 c =
    """ """ ++ (if not c.boolean then ("""NO""") else "") ++ """
 """ ++ (if not c.boolean then ("""WAY""") else "") ++ """
"""

type alias Context9 a =
    { a
        | boolean : Bool
    }

suite9 : Test
suite9 = test """Indented Inline Sections — Single-line sections should not alter surrounding whitespace.""" <|
    \_ ->
    { boolean =
        False
    }
    |> render9
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

render10 : Context10 a -> String
render10 c =
    """| This Is
""" ++ (if not c.boolean then ("""|
""") else "") ++ """| A Line
"""

type alias Context10 a =
    { a
        | boolean : Bool
    }

suite10 : Test
suite10 = test """Standalone Lines — Standalone lines should be removed from the template.""" <|
    \_ ->
    { boolean =
        False
    }
    |> render10
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

render11 : Context11 a -> String
render11 c =
    """| This Is
""" ++ (if not c.boolean then ("""|
""") else "") ++ """| A Line
"""

type alias Context11 a =
    { a
        | boolean : Bool
    }

suite11 : Test
suite11 = test """Standalone Indented Lines — Standalone indented lines should be removed from the template.""" <|
    \_ ->
    { boolean =
        False
    }
    |> render11
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

render12 : Context12 a -> String
render12 c =
    """|
""" ++ (if not c.boolean then ("""""") else "") ++ """|"""

type alias Context12 a =
    { a
        | boolean : Bool
    }

suite12 : Test
suite12 = test """Standalone Line Endings — "\r\n" should be considered a newline for standalone tags.""" <|
    \_ ->
    { boolean =
        False
    }
    |> render12
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

render13 : Context13 a -> String
render13 c =
    (if not c.boolean then ("""^""") else "") ++ """
/"""

type alias Context13 a =
    { a
        | boolean : Bool
    }

suite13 : Test
suite13 = test """Standalone Without Previous Line — Standalone tags should not require a newline to precede them.""" <|
    \_ ->
    { boolean =
        False
    }
    |> render13
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

render14 : Context14 a -> String
render14 c =
    """^""" ++ (if not c.boolean then ("""
/
""") else "")

type alias Context14 a =
    { a
        | boolean : Bool
    }

suite14 : Test
suite14 = test """Standalone Without Newline — Standalone tags should not require a newline to follow them.""" <|
    \_ ->
    { boolean =
        False
    }
    |> render14
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

render15 : Context15 a -> String
render15 c =
    """|""" ++ (if not c.boolean then ("""=""") else "") ++ """|"""

type alias Context15 a =
    { a
        | boolean : Bool
    }

suite15 : Test
suite15 = test """Padding — Superfluous in-tag whitespace should be ignored.""" <|
    \_ ->
    { boolean =
        False
    }
    |> render15
    |> equal """|=|"""

