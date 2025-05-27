# elm-mustache
[Mustache templates](https://mustache.github.io/) in Elm.

There are two parts to this package:
- The Elm package with which you can parse and render mustache templates in the browser.
- The NPM package with which you can parse a mustache template in advance and generate a typesafe™ rendering function for use in the browser.

## Should I use the Elm package or the NPM package?

If you have `.mustache` templates already that you'd want to render with different hashes in the browser then you should probably use the NPM package.
If you have a use-case where you can't know in advance what the template will be, then you should use the Elm package.
If you don't already have `.mustache` templates ready and have found this package because you're looking for a nice templating experience in Elm, then you are probably better off writing some rendering functions in Elm directly with the help of multiline strings, the `++` operator.

# The Elm package

See https://package.elm-lang.org/packages/emmabastas/elm-mustache/latest/

# The NPM package

There are two types of rendering functions that can be created: A typesafe but more limited version (It can't iterate a section), and a full version.

Let's say you have a file `mytemplate.mustache`

```mustache
{{#section}}
Hello {{world}}!
{{/section}}
```

Then you can generate a typesafe rendering function with `elm-mustache ./mytemplate.mustache ./src/Mustache/MyTemplate.elm Mustache.MyTemplate`, which will give you the Elm module:

```elm
module Mustache.MyTemplate exposing (..)

render : Context a -> String
render c =
    (if c.section then ("""Hello """ ++ htmlEscape c.world ++ """!
""") else "")

type alias Context a =
    { a
        | section : Bool
        , world : String
    }
```

One the other hand, if you run `elm-mustache` again but with the `--full` flag you'd get the "full" version:

```elm
module Mustache.MyTemplate exposing (..)

import Json.Decode exposing (Value)
import Mustache exposing (htmlEscape, lookup, interpolate, section, invertedSection)

render : Value -> String
render json = (\context0 ->
    (section context0 ["section"] (\context1 ->"""Hello """ ++ htmlEscape (interpolate (lookup context1 ["world"])) ++ """!
"""))) [json]
```


While the typesafe version is better in the sense that you can never forget to supply some variable, and typos will be caught by at compile time, it is worse in the sense that it can't iterate a section. With the full version you can call it with:

```elm
import Json.Encode exposing (list, string)
render (object
    [ ("world", string "Earth")
    , ("world", string "Venus")
    , ("world", string "Pluto")
    ])
```

which will produce the string

```
Hello Earth!
Hello Venus!
Hello Pluto!
```

This is not possible with the typesafe version

## Why can't the typesafe version deal with lists?

When presented with the template

```
{{#section}}
{{foo}}?
{{bar}}!!
{{/section}}
```

I can't tell if the `foo` and `bar` variables are supposed to change for every iteration of the section, or if they should remain the same, in other words I don't know if the `Context` type should be

```elm
{ section :
    List { foo : String
         , bar : String
         }
}
```

or

```elm
{ section : Bool
, foo : String
, bar : String
}
```

or something in between.

# Summary of capabilities and spec compliance


|                                     | `elm-mustache` | `elm-mustache --full` | Elm package |
|-------------------------------------|----------------|-----------------------|-------------|
| variables                           | ✅             | ✅                    | ✅          |
| boolean section                     | ✅             | ✅                    | ✅          |
| list section                        | ❌             | ✅                    | ✅          |
| inverted section                    | ✅             | ✅                    | ✅          |
| set delimiters                      | ✅             | ✅                    | ✅          |
| lambdas                             | ❌             | ❌                    | ❌          |
| partials                            | ❌             | ❌                    | ✅          |
| inheritance                         | ❌             | ❌                    | ❌          |
| dynamic names                       | ❌             | ❌                    | ❌          |
| respects `\r\n`-style line endings? | ✅             | ✅                    | ❌          |

# Testing

`npx elm-test` to run all unit tests. All unit tests are generated from `./tests/specs/*.json` which have been fetched from the mustache test suite over at https://github.com/mustache/spec

# Contributing

Yes please :-)
Feel free to email me or open an issue.

# TODOs

**Nice-to-haves**
- [ ] `elm-codegen` generators to replace the CLI tool (in a separate package)
- [ ] Fuzzy tests.

**v2.0.0 roadmap**
- [ ] Use `Parser.Advanced` to provide useful error messages
- [ ] Fault-tolerant parsing -- always return a parsed template, along with a list of eventual parse errors.
    - [ ] What does the spec say about error recovery in parsing?
- [ ] Figure out what the rendering strategy is -- Right now, missing keys and so on are silently rendered as empty strings, either be explicit about this or do something else.
- [ ] Respect `\r\n`-style line endings.
- [ ] Use a `Hash` type instead of `Json.Value`.
- [ ] The `Context` Type is some sort of tree zipper, not a stack (make impossible state impossible.)
- [ ] Make the implementation be more along the make-impossible-states-impossible philosophy.

# License

GPLv3



