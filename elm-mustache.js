const Mustache = require("mustache")

module.exports = {
    makeTypesafeModule : makeTypesafeModule,
    makeFullModule : makeFullModule,
    _makeModule : _makeModule,
}

function makeTypesafeModule(moduleName, ast) {
    let s = ""
    for (let part of _makeModule(moduleName, "typesafe", ast, false)) {
        s = s + part
    }
    return s
}

function makeFullModule(moduleName, ast) {
    let s = ""
    for (let part of _makeModule(moduleName, "full", ast, false)) {
        s = s + part
    }
    return s
}

function *_makeModule(moduleName, type, ast, test) {
    if (test) {
        var tests = ast
    }

    yield "module " + moduleName + " exposing (..)\n"
        + "\n"

    if (type === "typesafe") {
        yield "import Mustache exposing (htmlEscape, lookup, interpolate, section, invertedSection)\n"
            + "\n"
    }

    if (type === "full") {
        yield "import Json.Decode as D exposing (Value)\n"
            + "import Json.Encode as E\n"
            + "import Mustache exposing (htmlEscape, lookup, interpolate, section, invertedSection)\n"
            + "\n"
    }

    if (typeof tests !== "undefined") {
        yield "import Test exposing (Test, test)\n"
            + "import Expect exposing (equal)\n"
            + "\n"
    }

    yield "comment : String\n"
        + "comment = \"\"\n"
        + "\n"
        + "setDelimiter : String -> String -> String\n"
        + "setDelimiter _ _ = \"\"\n"
        + "\n"

    if (typeof tests === "undefined") {
        const variables = {}

        for (let line of makeRenderFunction("render", "Context", ast, variables, type)) {
          yield line
        }

        yield "\n\n"

        if (type === "typesafe") {
            for (let line of makeContextType("Context", variables)) {
                yield line
            }

            yield "\n\n"
        }
        return
    }

    for (let i = 0; i < tests.length; i++) {
        const test = tests[i]
        const testName = test.name
        const testDesc = test.desc
        const data = test.data
        const template = test.template
        const expected = test.expected
        const ast = Mustache.parse(template)

        const renderName = "render" + i
        const contextName = "Context" + i
        const suiteName = "suite" + i

        const variables = {}

        yield "\n\n{- "
            + testName
            + "\nThe template is:\n\n---\n"
            + template.replaceAll("\t", "\\t")
            + "\n---\nThe data is:\n\n---\n"
            + JSON.stringify(data, null, 4)
            + "\n---\n-}\n\n"

        for (let line of makeRenderFunction(renderName, contextName, ast, variables, type)) {
          yield line
        }

        yield "\n\n"

        if (type === "typesafe") {
            for (let line of makeContextType(contextName, variables)) {
                yield line
            }

            yield "\n\n"
        }

        for (let line of makeTest(renderName, contextName, suiteName, data, expected, testName + " — " + testDesc, type)) {
          yield line
        }

        yield "\n"
    }
}

function *makeRenderFunction(renderName, contextName, ast, variables, type) {
    if (type === "typesafe") {
        yield renderName + " : " + contextName + " a -> String\n"
            + renderName + " c =\n    "
    }
    else {
        yield renderName + " : Value -> String\n"
            + renderName + " json = (\\context0 ->\n    "
    }

    for (const line of makeRenderBody(ast, variables, type, 0)) {
        yield line
    }

    if (type === "full") {
        yield ") [json]"
    }
}

function *makeRenderBody(ast, variables, type, depth) {
    for (let i = 0; i < ast.length; i++) {

        const currentSpan = ast[i]
        const nodeType = currentSpan[0]
        let content = currentSpan[1]

        if (type === "typesafe" && ["name", "&", "#", "^"].includes(nodeType)) {
            addVariable(variables, content, nodeType)
        }

        //
        // nodeType === "text"
        //
        if (nodeType === "text") {
            yield elmMultilineString(content)
        }
        //
        // nodeType === "name"
        //
        else if (nodeType === "name" && content !== "." && type === "typesafe") {
            yield "htmlEscape c." + elmVariableCase(content)
        }
        else if (nodeType === "name" && content === "." && type === "typesafe") {
            yield "htmlEscape c.implicit"
        }
        else if (nodeType === "name" && content !== "." && type === "full") {
            yield "htmlEscape (interpolate (lookup context" + depth + " [" +
                content
                .split(".")
                .map(elmString)
                .join(", ")
                + "]))"
        }
        else if (nodeType === "name" && content === "." && type === "full") {
            yield "htmlEscape (interpolate (lookup context" + depth + " []))"
        }
        //
        // nodeType === "&"
        //
        else if (nodeType === "&" && content !== "." && type === "typesafe") {
            yield "c." + elmVariableCase(content)
        }
        else if (nodeType === "&" && content === "." && type === "typesafe") {
            yield "c.implicit"
        }
        else if (nodeType === "&" && content !== "." && type === "full") {
            yield "interpolate (lookup context" + depth + " [" +
                content
                .split(".")
                .map(elmString)
                .join(", ")
                + "])"
        }
        else if (nodeType === "&" && content === "." && type === "full") {
            yield "interpolate (lookup context" + depth + " [])"
        }
        //
        // nodeType === "#"
        //
        else if (nodeType === "#" && type === "typesafe") {
            if (content === ".") {
                yield "(if c.implicit then ("
            } else {
                yield "(if c." + elmVariableCase(content) + " then ("
            }

            if (currentSpan[4].length === 0) {
                currentSpan[4].push (["text", ""])
            }

            for (const line of makeRenderBody(currentSpan[4], variables, type, depth + 1)) {
                yield line
            }

            yield ") else \"\")"
        }
        else if (nodeType === "#" && type === "full") {
            var path
            if (content === ".") {
                path = ""
            } else {
                path = content
                    .split(".")
                    .map(elmString)
                    .join(", ")
            }

            yield "(section context" + depth + " [" + path + "] (\\context" + (depth + 1) + " ->"

            if (currentSpan[4].length === 0) {
                currentSpan[4].push (["text", ""])
            }

            for (const line of makeRenderBody(currentSpan[4], variables, type, depth + 1)) {
                yield line
            }

            yield "))"
        }
        //
        // nodeType === "^"
        //
        else if (nodeType === "^" && type === "typesafe") {
            if (content === ".") {
                yield "(if not c.implicit then ("
            } else {
                yield "(if not c." + elmVariableCase(content) + " then ("
            }

            if (currentSpan[4].length === 0) {
                currentSpan[4].push (["text", ""])
            }

            for (const line of makeRenderBody(currentSpan[4], variables, type, depth + 1)) {
                yield line
            }
            yield ") else \"\")"
        }
        else if (nodeType === "^" && type === "full") {
            var path
            if (content === ".") {
                path = ""
            } else {
                path = content
                    .split(".")
                    .map(elmString)
                    .join(", ")
            }

            yield "(invertedSection context" + depth + " [" + path + "] (\\context" + (depth + 1) + " ->"

            if (currentSpan[4].length === 0) {
                currentSpan[4].push (["text", ""])
            }

            for (const line of makeRenderBody(currentSpan[4], variables, type, depth + 1)) {
                yield line
            }

            yield "))"
        }
        else if (nodeType === "!") {
            yield "comment"
        }
        else if (nodeType === "=") {
            const [left, right] = content.split(" ")
            yield "(setDelimiter \"" + left + "\" \"" + right + "\")"
        }
        else if (nodeType === ">") {
            throw new Error("Partials are not supported")
        }
        else {
          throw new Error ("unknown node type" + nodeType)
        }


        if (typeof ast[i + 1] !== "undefined") {
            yield " ++ "
        }
    }
}

function *makeContextType(contextName, variables) {
  let length = 0
  for (var varName in variables) {
    if (Object.prototype.hasOwnProperty.call(variables, varName)) {
      length ++
    }
  }

  if (length === 0) {
    yield "type alias " + contextName + " a = { dummy : a }\n"
    return
  }

  yield "type alias " + contextName + " a =\n"
      + "    { a"

  let delim = "\n        | "

  for (var varName in variables) {
    if (Object.prototype.hasOwnProperty.call(variables, varName)) {

      const varType = variables[varName]

      if (varType === "name") {
        yield delim + elmVariableCase(varName) + " : String"
      } else if (varType === "&") {
        yield delim + elmVariableCase(varName) + " : String"
      } else if (varType === "#") {
        yield delim + elmVariableCase(varName) + " : Bool"
      } else if (varType === "^") {
        yield delim + elmVariableCase(varName) + " : Bool"
      } else {
        throw new Error("Unexpected varType: " + varType + " (varName is " + varName + ")")
      }

      delim = "\n        , "
    }
  }

  yield "\n    }"
}

function *makeTest(renderName, contextName, suiteName, data, expected, desc, type) {
    var dataStr
    if (type === "typesafe") {
        if (["boolean", "number", "string"].includes(typeof data)) {
            dataStr = "{ implicit = " + elmLiteral(data, 4, false) + " }"
        }
        else if (typeof data === "object") {
            dataStr = elmLiteral(data, 4, false)
        }
        else {
            throw new Error(data)
        }
    }
    else {
        dataStr = elmJson(data, 4, true)
    }

    yield suiteName + " : Test\n"
        + suiteName + " = test \"\"\"" + desc + "\"\"\" <|\n"
        + "    \\_ ->\n"
        + "    "+ dataStr + "\n"
        + "    |> " + renderName + "\n"
        + "    |> equal " + elmMultilineString(expected) + "\n"

}

function elmJson(data, indent, needsParen) {
    if (data === null) {
        return "E.null"
    }
    else if (data === true) {
        return "(E.bool True)"
    }
    else if (data === false) {
        return "(E.bool False)"
    }
    if (typeof data === "number" && Number.isInteger(data)) {
        return "(E.int " + data + ")"
    }
    if (typeof data === "number" && !Number.isInteger(data)) {
        return "(E.float " + data + ")"
    }
    if (typeof data === "string") {
        return "(E.string " + elmMultilineString(data) + ")"
    }
    else if (typeof data === "object" && Array.isArray(data)) {
        let whitespace = " ".repeat(indent)

        const a = []
        for (const elem of data) {
            a.push(elmJson(elem, indent + 2, false))
        }

        if (a.length === 0) {
            return "(E.list identity [])"
        }
        if (a.length === 1) {
            return "(E.list identity [" + a[0] + "])"
        }
        return "(E.list identity [ " + a.join("\n" + whitespace + ", ") + "\n" + whitespace + "])"
    }
    else if (typeof data === "object") {
        const whitespace = " ".repeat(indent)

        const a = []
        for (let key of keys(data)) {
            a.push("( " + elmString(key) + "\n  " + whitespace + "  , " + elmJson(data[key], indent + 4, false) + "\n" + whitespace + "    )")
        }

        if (a.length === 0) {
            return "(E.object [])"
        }

        return "(E.object\n" + whitespace + "  [ " + a.join("\n  " + whitespace + ", ") + "\n" + whitespace + "  ])"
    }
    else {
      throw new Error("Can't make a " + typeof data + " into an Elm Value")
    }
}

function elmLiteral(data, indent, needsParen) {
    if (data === true) {
        return "True"
    }
    if (data === false) {
        return "False"
    }
    if (typeof data === "number" && Number.isInteger(data)) {
        if (needsParen) {
            return "(String.fromInt " + data + ")"
        }
        return "String.fromInt " + data
    }
    if (typeof data === "number" && !Number.isInteger(data)) {
        if (needsParen) {
            return "(String.fromFloat " + data + ")"
        }
        return "String.fromFloat " + data
    }
    if (typeof data === "string") {
      return elmMultilineString(data)
    }
    else if (typeof data === "object" && Array.isArray(data)) {
        let whitespace = " ".repeat(indent)

        const a = []
        for (const elem of data) {
            a.push(elmLiteral(elem, indent + 2, false))
        }

        if (a.length === 0) {
            return "[]"
        }
        if (a.length === 1) {
            return "[" + a[0] + "]"
        }
        return "[ " + a.join("\n" + whitespace + ", ") + "\n" + whitespace + "]"
    }
    else if (typeof data === "object") {
        data = flattenObject(data)

        let whitespace = " ".repeat(indent)

        const a = []
        for (var field of keys(data)) {
            a.push(elmVariableCase(field) + " =\n    " + whitespace +
                   elmLiteral(data[field], indent + 4, false))
        }

        if (a.length === 0) {
            return "{ dummy = () }"
        }
        return "{ " + a.join("\n" + whitespace + ", ") + "\n" + whitespace + "}"
    }
    else {
      throw new Error("Can't make a " + typeof data + " into an Elm literal")
    }
}

function addVariable(variables, varName, varType) {
  if (typeof variables[varName] !== "undefined" && variables[varName] !== varType) {
    throw new Error("Incompatible variable types; " + variables[varName] + " and " + varType)
  }

  variables[varName] = varType
}

function elmString(s) {
    return "\"" + s.replaceAll("\"", "\\\"").replaceAll("\n", "\\n") + "\""
}

function elmMultilineString(s) {
    return "\"\"\"" + s.replaceAll("\"", "\\\"") + "\"\"\""
}

// TODO
function elmVariableCase(name) {
  const leading = name.charCodeAt(0)
  if(leading >= 48 && leading <= 57) {
    name = "f" + name
  }


  // TODO Implicit iterators
  if (name === ".") {
    return "implicit"
  }

  return name.replaceAll(".", "_")
}

function keys(obj) {
    const a = []
    for (var key in obj) {
        if (Object.prototype.hasOwnProperty.call(obj, key)) {
            a.push(key)
        }
    }
    return a
}

function flattenObject(obj, ctx=[], root={}) {
    for (key of keys(obj)) {
        value = obj[key]
        if (typeof value === "object" && !Array.isArray(value)) {
            flattenObject(value, ctx = ctx.concat(key), root)
        } else {
            root[ctx.concat(key).join("_")] = value
        }
    }
    return root
}
