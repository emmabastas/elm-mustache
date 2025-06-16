{- Some notes for a fellow developer
   This module is divided in four parts:
   1) User facing functions
      - `parse`
      - `renderParsed`
      - `render`
   2) Parsing related
   3) Rendering related
   4) Finding the correct value to interpolate given a context.

   IMO there is one feature of the Mustache spec that makes this module way more
   obtuse and bug-prone than it would have been otherwise, and that is the idea
   of "standalone tags". These aren't documented in
   https://mustache.github.io/mustache.5.html, but there are plenty of unit tests
   pertaining to this feature.

   The basic idea is that, according to our intuition, both:

   "{{#section}}foo{{/section}}"

   and:

   """{{#section}}
   foo
   {{/section}}"""

   should render as "foo", makes sense! But implementing this is quite
   difficult when considering all edge cases. In the first template we simply
   "remove" the {{#section}} and {{/section}} bits, but in the second template
   we need also need to "remove" the newlines following the {{#section}} and
   preceding the {{/section}}. And it doesn't stop there, we need to handle
   "indented standalone" lines too!

   The reason this is in the spec is of course because it's the behavior a
   human would expect, but it's quite difficult to program (at least I didn't
   find an elegant way to do it functional-style).

   This feature is the reason you'll find the `renderAst_` function to be quite
   complicated, it's the reason the `Text` node takes a `List String` (where
   each element represents a line) instead of simply a `String` and it's the
   reason we have some `postWhitespace` and `preSubsectionWhitespace` in the
   records of the `Ast` variants.


   Another thing of note is that I'm using the `lookAhead` function from
   pithub/elm-parser-extra as well has having defined my own
   `chompUntilOneOf` which is like `chompUntil` but you use a list of parsers
   to decide when to stop chomping.
-}

module Mustache exposing
    ( Ast
    , Context
    , render
    , parse
    , renderParsed
    , htmlEscape
    , lookup
    , interpolate
    , section
    , invertedSection
    )

{-|

# Parse and render
@docs Ast, render, parse, renderParsed

# Partials

A string can be used as the template in a partial tag

    import Json.Encode as E

    template : String
    template = """
    <h2>Names</h2>
    {{#names}}
        {{> user}}
    {{/names}}
    """

    partial : String
    partial = "<strong>{{.}}</strong>"

    render template
        ( E.object
            [ ("names", E.list E.string [ "Alice", "Bob" ])
            , ("user", E.string partial)
            ]
        )
    --> [ ""
    --> , "<h2>Names</h2>"
    --> , "    <strong>Alice</strong>"
    --> , "    <strong>Bob</strong>"
    --> , ""
    --> , ""
    --> ]
    --> |> String.join "\n"
    --> |> Ok

# Niche usecases
@docs htmlEscape, Context, lookup, interpolate, section, invertedSection
-}

import Json.Decode as D exposing (Value)
import Json.Encode as E
import Parser exposing
    ( Parser
    , DeadEnd
    , run
    , (|.)
    , (|=)
    , succeed
    , problem
    , backtrackable
    , loop
    , Step (..)
    , end
    , token
    , chompIf
    , chompWhile
    , getChompedString
    , mapChompedString
    , oneOf
    , map
    , andThen
    {- The following functions have bugs in them and shouldn't be used, instead
       use equivalent functions from pithub/elm-parser-bug-workaround
    , chompUntil       -> chompUntilBefore
    , chompUntilEndOr  -> chompUntilEndOrBefore
    , lineComment      -> lineCommentBefore
    , multilineComment -> multilineCommentBefore
    -}
    )
import Parser.Workaround exposing
    ( chompUntilBefore
    , chompUntilEndOrBefore
    )
import Parser.Extra exposing (lookAhead)

{-| Parse — but don't render — a template. Useful if you want to use the same template many times with different hashes.
-}
parse : String -> Result (List DeadEnd) Ast
parse template = run parser ("\n" ++ template)
{- Why the `"\n" ++` bit? Well it makes it easier to handle
   an edge case. Consider the two templates:
   "\n{{!comment}}\nfoo"
   and
   "{{!comment}}\nfoo"

   Both comments are "standalone tags", for the first case we check that
   the comment is preceded and followed by newlines.

   For the second one we have to check if it's at the start of the string, annoying to have two cases! If we instead prepend a newline we get the templates:
   "\n\n{{!comment}}\nfoo"
   and
   "\n{{!comment}}\nfoo"

   and never need to check the special case, and we simply remove the extra
   newline when done rendering.

   I tried doing the same "trick" with appending a newline as well, but it didn't
   work for some reason..
 -}

{-| Once a template has been parsed with `parse` it can be rendered with this function.

    import Json.Encode as E

    ast : Ast
    ast = parse "Hello, {{.}}!" |> Result.withDefault []

    renderParsed ast (E.string "Pluto")  --> "Hello, Pluto!"

    renderParsed ast (E.string "Mercury")  --> "Hello, Mercury!"

-}
renderParsed : Ast -> Value -> String
renderParsed ast hash =
    renderAst ast [hash]
    |> (\s -> String.slice 1 (String.length s) s)

{-| This is probably the one function you'll want to use. Expects a mustache template string and a JSON object to use as a hash.

    import Json.Encode as E

    render "Hello, {{world}}!" (E.object [("world", E.string "Pluto")])  --> Ok "Hello, Pluto!"
-}
render : String -> Value -> Result (List DeadEnd) String
render template hash =
    parse template
    |> Result.map (\ast -> renderParsed ast hash)

{- The main AST type, the rest of the codebase makes assumptions about the
   structure of the AST that are not encoded in the type itself (i.e. there
   are some impossible states that are actually possible).

   The assumption is that the first and last nodes are always `Text` nodes,
   and also that all other nodes are interleaved with `Text` nodes. In other
   words, for any `Ast` the pattern `Text _ :: <Some non-Text node> :: Text _`
   should always match. Of course the Elm compiler wouldn't know about this,
   but we do ;)

   One could try to "make impossible states impossible" by redefining the AST to be

   ```
   type alias Ast =
       { nodes : List (List String, AstNode)
       , lastText : List String
       }
   ```

   and removing the `Text` variant from the `AstNode`

   However I can't be bothered with such a major refactoring.

   Another quirk is that in the AST, the first element in the list
   is the last node that was parsed. I.e. the template

   """hello {{world}}!
   foo"""

    is represented by this AST:

    [Text ["foo", "!"], Variable { ... , name = ["world"] }, Text ["Hello "]]
-}

{-| The AST type.
-}
type alias Ast = List AstNode

-- name "foo.bar.baz" == Name ["foo", "bar", "baz"]
type alias Name = List String

name : String -> Name
name s =
    case s of
        "." -> []
        _   -> String.split "." s

{-| The *context* is a list of *hashes* (both of these terms are specified in the mustache specification) with the head of the list being the innermost hash.
-}
type alias Context = List Value

{- All of the `postWhitespace` and `subsectionPreWhitespace` fields are used
   in the `renderAst_` function to figure out when tags should be considered
   "standalone tags".

   For example, the template

   """foo
       {{#section}}
   bar
   {{/section}}    (<- we have trailing whitespace here)
   baz"""

   is represented by this AST:

   [ Text ["baz"]
   , Section
        { name = ["section"]
        , postWhitespace = "    \n"
        , subsection = [Text ["bar"]]
        , subsectionPreWhitespace = "\n"
        }
   ,Text ["      ", "foo"]
   ]
-}
type AstNode
    = Text TextNode
    | Variable
        { name : Name
        , escapeHtml : Bool
        }
    | Section
        { name : Name
        , subsection : Ast
        , postWhitespace : String
        , subsectionPreWhitespace : String
        }
    | InvertedSection
        { name : Name
        , subsection : Ast
        , postWhitespace : String
        , subsectionPreWhitespace : String
        }
    | Partial
        { name : Name
        , indentation : Int
        , postWhitespace : String
        }
    -- The `Comment` and `SetDelimiter` tags don't look like they carry any
    -- information in the AST. However, `renderAst_` uses them to remove
    -- newlines where a user would expect them to be removed (standalone tags).
    -- See the documentation of `renderAst_` for more info.
    | Comment
        { postWhitespace : String
        }
    | SetDelimiter
        { postWhitespace : String
        }

{- PARSING
   Here we are concerned only with how to turn a template string
   into an AST.
-}

type alias ParserState =
    { stack : List (Name, Ast, Ast -> String -> AstNode)
    , current : Ast
    , delim : Delimiter
    }

type alias Delimiter =
    { left : String
    , right : String
    }

addNode : AstNode -> ParserState -> ParserState
addNode node state = { state | current = node :: state.current }

intoSection : Name -> (Ast -> String -> AstNode) -> ParserState -> ParserState
intoSection sectionName sectionType state =
    { stack = (sectionName, state.current, sectionType) :: state.stack
    , current = []
    , delim = state.delim
    }

leaveSection : Name -> String -> ParserState -> ParserState
leaveSection leaveName postWhitespace state =
    case state.stack of
        (sectionName, ast, sectionType) :: xs ->
            if leaveName == sectionName then
                { stack = xs
                , current = (sectionType state.current postWhitespace) :: ast
                , delim = state.delim
                }
            {- This happens if we have a template where the opening and closing tags
               have different names:
               {{# section }}
                  foo
               {{/ section }}

               What should we do in this situation? For now, we just pretend the the
               Names actually do match up
            -}
            else
                { stack = xs
                , current = (sectionType state.current postWhitespace) :: ast
                , delim = state.delim
                }

        {-   This happens if we have a template where there's a closing tag with no accompanying opening tag:
           foo{{/section}}

           What should we do in this situation?
           We do nothing?
        -}
        [] ->
            state

parser : Parser Ast
parser =
    textNode { left = "{{", right = "}}" }
    |> andThen
        (\tn ->
            parser_
                { stack = []
                , current = [Text tn]
                , delim = { left = "{{", right = "}}" }
                }
        )
    |> map (\state -> state.current)

parser_ : ParserState -> Parser ParserState
parser_ initialState =
    loop initialState
        (\state ->
            oneOf
                [ (tag state.delim)
                  |> andThen
                      (\t -> case t of
                            Nothing ->
                                succeed state

                            Just (VariableTag x) ->
                                addNode (Variable x) state
                                |> succeed

                            Just (PartialTag partialName) ->
                                let
                                    indentation = case state.current of
                                        {- In order for indentation to be relevant
                                           we need to partial tag to be preceeded
                                           by one whole line containing only spaces.
                                           `TextNode s _` means that some other tag
                                           is on the same line, so instead we need
                                           `(TextNode s (_ :: _)) :: _`
                                        -}
                                        Text (TextNode s (_ :: _)) :: _ ->
                                            if String.all ((==) ' ') s then
                                                String.length s
                                            else
                                                0

                                        _ ->
                                            0
                                in
                                chompWhile ((==) ' ')
                                |. oneOf
                                    [ token "\n"
                                    , succeed ()
                                    ]
                                |> mapChompedString
                                    (\s _ ->
                                        addNode
                                            (Partial
                                                { name = partialName
                                                , indentation = indentation
                                                , postWhitespace = s
                                                }
                                            )
                                            state
                                    )

                            Just (SectionStart sectionName) ->
                                chompWhile ((==) ' ')
                                |. oneOf
                                    [ token "\n"
                                    , succeed ()
                                    ]
                                |> mapChompedString
                                    (\s _ ->
                                        intoSection
                                            sectionName
                                            (\subsection postWhitespace ->
                                                { name = sectionName
                                                , subsection = subsection
                                                , postWhitespace = postWhitespace
                                                , subsectionPreWhitespace = s
                                                }
                                                |> Section
                                            )
                                            state
                                    )

                            Just (InvertedSectionStart sectionName) ->
                                chompWhile ((==) ' ')
                                |. oneOf
                                    [ token "\n"
                                    , succeed ()
                                    ]
                                |> mapChompedString
                                    (\s _ ->
                                        intoSection
                                            sectionName
                                            (\subsection postWhitespace ->
                                                { name = sectionName
                                                , subsection = subsection
                                                , postWhitespace = postWhitespace
                                                , subsectionPreWhitespace = s
                                                }
                                                |> InvertedSection
                                            )
                                            state
                                    )

                            Just (SectionEnd sectionName) ->
                                chompWhile ((==) ' ')
                                |. oneOf
                                    [ token "\n"
                                    , succeed ()
                                    ]
                                |> mapChompedString
                                    (\postWhitespace _ ->
                                        leaveSection sectionName postWhitespace state
                                    )

                            Just CommentTag ->
                                chompWhile ((==) ' ')
                                |. oneOf
                                    [ token "\n"
                                    , succeed ()
                                    ]
                                |> mapChompedString
                                    (\s _ ->
                                        addNode
                                            (Comment { postWhitespace = s })
                                            state
                                    )

                            Just (SetDelimiterTag newDelimiter) ->
                                chompWhile ((==) ' ')
                                |. oneOf
                                    [ token "\n"
                                    , succeed ()
                                    ]
                                |> mapChompedString
                                    (\s _ ->
                                        addNode
                                            (SetDelimiter { postWhitespace = s })
                                            { state | delim = newDelimiter }
                                    )
                      )
                  |> andThen
                      (\newState ->
                          succeed (\tn -> Loop (addNode (Text tn) newState))
                          |= textNode newState.delim
                      )
                  |> backtrackable
                , succeed ()
                  |. chompUntilOneOf [ end ]
                  |> getChompedString
                  |> map
                    (\s ->
                        if s == "" then
                            Done state
                        else
                            Done (addNode (Text (TextNode s [])) state))
                ])

{-
  Represents a pice of plain text broken into a head line and tail lines.
  The head line is actually the last line of the text. So
  "foo\nbar\nbaz" -> TextNode "baz" ["bar", "foo"]
-}
type TextNode = TextNode String (List String)

isEmpty : TextNode -> Bool
isEmpty (TextNode head tail) = head == "" && tail == []

emptyTextNode : TextNode
emptyTextNode = TextNode "" []

endsInStandalone : TextNode -> Bool
endsInStandalone (TextNode s _) = String.all ((==) ' ') s

renderTextNode : TextNode -> String
renderTextNode (TextNode s ss) = String.join "\n" (List.reverse (s :: ss))

renderTextNodeTrimEnd : TextNode -> String
renderTextNodeTrimEnd (TextNode _ ss) = String.join "\n" (List.reverse ss)

textNode : Delimiter -> Parser TextNode
textNode delim =
    chompUntilOneOf
        [ map (always (\s -> succeed (TextNode s []))) end
        , map (always (\s -> succeed (TextNode s []))) (lookAhead (token delim.left))
        , map (always (\s -> textNode_ delim (String.slice 0 -2 s))) (token "\r\n")
        , map (always (\s -> textNode_ delim (String.slice 0 -1 s))) (token "\n")
        ]
        |> mapChompedString (\s f -> f s)
        |> andThen (\a -> a)

textNode_ : Delimiter -> String -> Parser TextNode
textNode_ delim head_ =
    loop (head_, [])
        (\(head, tail) ->
            chompUntilOneOf
                [ map (always (\s -> Done (s, head :: tail))) end
                , map (always (\s -> Done (s, head :: tail))) (lookAhead (token delim.left))
                , map (always (\s -> Loop (String.slice 0 -2 s, head :: tail))) (token "\r\n")
                , map (always (\s -> Loop (String.slice 0 -1 s, head :: tail))) (token "\n")
                ]
                |> mapChompedString (\s f -> f s)
        )
        |> map (\(head, tail) -> TextNode head tail)

type Tag
    = VariableTag
        { name : Name
        , escapeHtml : Bool
        }
    | PartialTag Name
    | SectionStart Name
    | InvertedSectionStart Name
    | SectionEnd Name
    | CommentTag
    | SetDelimiterTag Delimiter

tag : Delimiter -> Parser (Maybe Tag)
tag delim =
    succeed identity
        |. token delim.left
        |= oneOf
            -- {{{ }}}
            [ if delim == { left = "{{", right = "}}" } then
                andThen
                    (\_ ->
                        tagName "}}}"
                        |. token "}}}"
                        |> map
                            (\variableName ->
                                Just <| VariableTag
                                    { name = variableName
                                    , escapeHtml = False
                                    }
                            )
                    )
                    (token "{")
                else
                    problem "Triple mustache no supported with custom delimiters"
            -- {{& }}
            , andThen
                (\_ ->
                    tagName delim.right
                    |. token delim.right
                    |> map
                        (\variableName ->
                            Just <| VariableTag
                                { name = variableName
                                , escapeHtml = False
                                }
                        )
                )
                (token "&")
            -- {{# }}
            , andThen
                (\_ ->
                    succeed (Just << SectionStart)
                    |= tagName delim.right
                    |. token delim.right
                )
                (token "#")
            -- {{^ }}
            , andThen
                (\_ ->
                    succeed (Just << InvertedSectionStart)
                    |= tagName delim.right
                    |. token delim.right
                )
                (token "^")
            -- {{/ }}
            , andThen
                (\_ ->
                    succeed (Just << SectionEnd)
                    |= tagName delim.right
                    |. token delim.right
                )
                (token "/")
            -- {{> }}
            , andThen
                (\_ ->
                    tagName delim.right
                    |. token delim.right
                    |> map
                        (\partialName ->
                            Just <| PartialTag partialName
                        )
                )
                (token ">")
            -- {{! }}
            , andThen
                (\_ ->
                    succeed (Just CommentTag)
                    |. tagName delim.right
                    |. token delim.right
                )
                (token "!")
            -- {{= }}
            , andThen
                (\_ ->
                    oneOf
                        [ succeed (Just << SetDelimiterTag)
                          |= delimiter delim.right
                          |> backtrackable
                        , succeed Nothing
                        ]

                )
                (token "=")
            -- {{ }}
            , tagName delim.right
              |. token delim.right
              |> map
                  (\variableName ->
                      Just <| VariableTag
                          { name = variableName
                          , escapeHtml = True
                          }
                  )
            ]

tagName : String -> Parser Name
tagName ending =
    succeed ()
    |. chompUntilBefore ending
    |> getChompedString
    |> map (\s -> name (String.trim s))

delimiter : String -> Parser Delimiter
delimiter rightDelim =
    (succeed ()
    |. chompWhile ((==) ' ')
    |. chompWhile (\c -> c /= ' ' && c /= '=')
    |> getChompedString)
    |. oneOf
        [ (token " ")
        , problem "equal signs are not allowed in delimiters"
        ]
    |> andThen
        (\left ->
            (getChompedString <| chompUntilBefore "=")
            |. token "="
            |. oneOf
                [ token rightDelim
                , problem "equal signs are not allowed in delimiters"
                ]
            |> map (\right -> {left = String.trimLeft left, right = String.trim right})
        )

chompUntilOneOf : List (Parser a) -> Parser a
chompUntilOneOf ps =
    loop ()
        (\_ ->
            oneOf
                [ map Done (oneOf ps)
                , map Loop (chompIf (always True))
                ]
        )

{- RENDER AST
   Here we are consearned with how to turn an Ast into a string
   given some context.
-}

renderAst : Ast -> Context -> String
renderAst ast context = renderAst_ True (List.reverse ast) context

renderAst_ : Bool -> Ast -> Context -> String
renderAst_ toplevel ast context =
    case ast of
        [] -> ""

        -- COMMENT

        Text tn :: Comment r :: xs ->
            renderCommentOrSetDelimiter
                toplevel
                context
                tn
                r.postWhitespace
                xs

        -- SET DELIMITER

        Text tn :: SetDelimiter r :: xs ->
            renderCommentOrSetDelimiter
                toplevel
                context
                tn
                r.postWhitespace
                xs

        -- SECTION

        Text tn :: Section r :: xs ->
            case r.subsection of
                Text (TextNode t tt) :: ys ->
                    renderSomeSection
                        { toplevel = toplevel
                        , context = context
                        , sectionName = r.name
                        , tn = tn
                        , subsection =
                            { t = t
                            , tt = tt
                            , ys = ys
                            , preWhitespace = r.subsectionPreWhitespace
                            }
                        , postWhitespace = r.postWhitespace
                        , xs = xs
                        , renderSection = section
                        }

                -- This should never match
                _ ->
                    ""


        -- INVERTED SECTION

        Text tn :: InvertedSection r :: xs ->
            case r.subsection of
                Text (TextNode t tt) :: ys ->
                    renderSomeSection
                        { toplevel = toplevel
                        , context = context
                        , sectionName = r.name
                        , tn = tn
                        , subsection =
                            { t = t
                            , tt = tt
                            , ys = ys
                            , preWhitespace = r.subsectionPreWhitespace
                            }
                        , postWhitespace = r.postWhitespace
                        , xs = xs
                        , renderSection = invertedSection
                        }

                -- This should never match
                _ ->
                    ""

        -- TEXT

        Text tn :: xs ->
            renderTextNode tn
            |> (\s -> s ++ renderAst_ toplevel xs context)

        -- VARIABLE

        Variable r :: xs ->
            interpolate (lookup context r.name)
            |> (if r.escapeHtml then htmlEscape else (\s -> s))
            |> (\s -> s ++ renderAst_ toplevel xs context)

        -- PARTIAL

        Partial r :: xs ->
            (case lookup context r.name |> Maybe.map (D.decodeValue D.string) of
                -- The partial key has corresponding data in the context, and it
                -- is a string.
                Just (Ok partialSource) ->
                    let
                        lines = String.split "\n" partialSource
                        indentedPartialSource =
                            case lines of
                                [] ->
                                    ""

                                l :: [] ->
                                    l

                                l :: ls ->
                                    ls
                                    |> List.map (\s ->
                                        if s == "" then
                                            ""
                                        else
                                            String.repeat r.indentation " " ++ s)
                                    |> String.join "\n"
                                    |> \s -> l ++ "\n" ++ s
                    in
                    case parse indentedPartialSource of
                        -- The partial is a valid mustache template.
                        Ok partialAst ->
                            renderAst partialAst context
                            |> (\s -> String.slice 1 (String.length s) s)
                            |> E.string
                            |> Just
                        -- The partial is not a valid mustache template.
                        Err _ ->
                            Nothing
                -- The partial key has corresponding data in the the context, but
                -- it's not a string. What should we do?
                Just (Err _) -> Nothing
                -- The partial key has no corresponding data in the context.
                Nothing -> Nothing
            )
            |> interpolate
            |> \s ->
                if r.indentation /= 0 && String.endsWith "\n" r.postWhitespace then
                    s ++ renderAst_ toplevel xs context
                else
                    s ++ r.postWhitespace ++ renderAst_ toplevel xs context

        -- These patterns should never match

        Comment r :: xs -> ""
        SetDelimiter r :: xs -> ""
        Section r :: xs -> ""
        InvertedSection r :: xs -> ""

renderCommentOrSetDelimiter :
    Bool
    -> Context
    -> TextNode
    -> String
    -> Ast
    -> String
renderCommentOrSetDelimiter toplevel context tn postWhitespace xs =
    let
        preStandalone = endsInStandalone tn

        postStandalone =
            String.endsWith "\n" postWhitespace
            || xs == [Text emptyTextNode]
    in
    if preStandalone && postStandalone then
        renderTextNodeTrimEnd tn
        ++ "\n"
        ++ renderAst_ toplevel xs context
    else
        renderTextNode tn
        ++ postWhitespace
        ++ renderAst_ toplevel xs context

renderSomeSection :
    { toplevel : Bool
    , context : Context
    , sectionName : Name
    , tn : TextNode
    , subsection :
        { t : String
        , tt : List String
        , ys : Ast
        , preWhitespace : String
        }
    , postWhitespace : String
    , xs : Ast
    , renderSection : Context -> Name -> (Context -> String) -> String
    } -> String
renderSomeSection { sectionName, toplevel, context, tn, subsection, postWhitespace, xs, renderSection } =
    let
        (TextNode _ ss) = tn
        { t, tt, ys, preWhitespace } = subsection
        openingTagStandalone =
            endsInStandalone tn
            && String.endsWith "\n" subsection.preWhitespace
        closingTagStandalone =
            List.length tt > 0
            && String.all ((==) ' ') t
            && (String.endsWith "\n" postWhitespace
                || xs == [Text emptyTextNode])

        first =
            if openingTagStandalone then
                renderTextNodeTrimEnd tn
                ++ if ss == [] then "" else "\n"
            else
                renderTextNode tn
                ++ subsection.preWhitespace

        second =
            if closingTagStandalone then
                renderSection context sectionName (renderAst_ False (List.reverse <| Text (TextNode (List.head tt |> Maybe.withDefault "") (List.tail tt |> Maybe.withDefault [])) :: ys))
                ++ "\n"
            else
                renderSection context sectionName (renderAst_ False (List.reverse <| Text (TextNode t tt) :: ys))
                ++ postWhitespace

    in
    first ++ second ++ renderAst_ toplevel xs context

{-| Escape the special HTML characters `&`, `"`, `<` and `>`. This function is used when rendering values in normal variable tags.
-}
htmlEscape : String -> String
htmlEscape s =
    s
    |> String.replace "&" "&amp;"
    |> String.replace "\"" "&quot;"
    |> String.replace "<" "&lt;"
    |> String.replace ">" "&gt;"

-- The functions bellow are all about finding the appropriate string
-- to insert into a template "hole" based off of the current context.

{-| Given a *context*, lookup a value in it. The lookup rules follows the ones outlined in the mustache specification.
-}
lookup : Context -> Name -> Maybe Value
lookup context variableName = case (context, variableName) of
    ([], _)                 -> Nothing
    (json :: xs, []) -> Just json
    (json :: xs, field :: ys) ->
        case lookup_ json field of
            Nothing -> lookup xs variableName
            Just j  -> lookup [j] ys

lookup_ : Value -> String -> Maybe Value
lookup_ json field =
    json
    |> D.decodeValue (D.at [field] D.value)
    |> Result.toMaybe

{-| Interpolate a JSON value into a string. This function is used when inserting a JSON value into a mustache variable.
-}
interpolate : Maybe Value -> String
interpolate m =
    D.oneOf
        [ D.string
        , D.null ""
        , D.map (\b -> if b then "true" else "false") D.bool
        , D.map String.fromInt D.int
        , D.map String.fromFloat D.float
        , D.map (\xs -> List.map (\v -> interpolate (Just v)) xs |> String.join "") (D.list D.value)
        , D.map (\xs -> "") (D.keyValuePairs D.value)
        ]
    |> \d -> D.decodeValue d (Maybe.withDefault E.null m)
    |> Result.withDefault "ERROR!!"

{-| Render a mustache section. This is only intended to be used by preparsed templates generated with the `elm-mustache` CLI tool
-}
section : Context -> Name -> (Context -> String) -> String
section context path inner = case lookup context path of
    Nothing -> ""
    Just sectionJson ->
        if not (isTruthy sectionJson) then
            ""
        else
            D.oneOf
                [ D.map (List.map (\json -> inner (json :: context))) (D.list D.value)
                  |> D.map (String.join "")
                , D.map (\json -> inner (json :: context)) D.value
                ]
            |> \d -> D.decodeValue d sectionJson
            |> Result.withDefault "ERROR!!"

{-| Render a mustache inverted section. This is only intended to be used by preparsed templates generated with the `elm-mustache` CLI tool
-}
invertedSection : Context -> Name -> (Context -> String) -> String
invertedSection context path inner = case lookup context path of
    Nothing -> inner context
    Just sectionJson ->
        if isFalsy sectionJson then
            inner context
        else
            ""

isTruthy : Value -> Bool
isTruthy json =
    D.oneOf
        [ D.map (\_ -> True) D.string
        , D.null False
        , D.bool
        , D.map (\_ -> True) D.int
        , D.map (\_ -> True) D.float
        , D.map (\l -> List.length l > 0) (D.list D.value)
        , D.map (\_ -> True) (D.keyValuePairs D.value)
        ]
    |> \d -> D.decodeValue d json
    |> Result.withDefault False

isFalsy : Value -> Bool
isFalsy json =
    D.oneOf
        [ D.map (\_ -> False) D.string
        , D.null True
        , D.map not D.bool
        , D.map (\_ -> False) D.int
        , D.map (\_ -> False) D.float
        , D.map (\l -> List.length l == 0) (D.list D.value)
        , D.map (\_ -> False) (D.keyValuePairs D.value)
        ]
    |> \d -> D.decodeValue d json
    |> Result.withDefault False

